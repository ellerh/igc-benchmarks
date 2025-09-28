
import Dates
import Plots
import Statistics
import Printf: @sprintf

struct Event
    time::Float64               # seconds (since some base e.g. Unix epoch)
    name::Symbol
    args::Union{Nothing,String}
end

struct GCPauseIter
    events::Vector{Event}
end

struct GCPause
    start::Float64              # seconds since start of trace
    duration::Float64
end

struct Trace
    duration::Float64
    pauses::Vector{GCPause}
    label::String
end

stp_line_rx = Regex("\
^\\[\
([[:digit:]]{4})-([[:digit:]]{2})-([[:digit:]]{2}) \
([[:digit:]]{2}):([[:digit:]]{2}):([[:digit:]]{2}).([[:digit:]]{9})\
\\] ([^ ]+)( [^\n]+)?")

parse_stp_line(line::AbstractString)::Event =
    let (year, month, day, hour, minute, second, ns, name, args) =
            match(stp_line_rx, line).captures
        datetime = Dates.DateTime(
            parse(Int, year),
            parse(Int, month),
            parse(Int, day),
            parse(Int, hour),
            parse(Int, minute),
            parse(Int, second),
        )
        time = (Dates.datetime2unix(datetime) + parse(Int, ns) * 1e-9)
        Event(time, Symbol(name), args)
    end

parse_stp_file(filename) = map(parse_stp_line, eachline(filename))

telemetry_line_rx =
    r"^([[:xdigit:]]{16}) [[:xdigit:]]{4} ([^ ]+) +([^\n]+)$"

const tsc_frequency = 2.1948e9 # in Hz

parse_telemetry_line(line)::Event =
    let (tsc, name, args) = match(telemetry_line_rx, line).captures
        Event(
            parse(UInt, tsc, base = 16) / tsc_frequency,
            Symbol(name),
            args,
        )
    end

parse_telemetry(filename) = map(parse_telemetry_line, eachline(filename))

is_no_work_was_done_event(e) =
    e.name == :ArenaPollEnd &&
    !isnothing(match(r"workWasDone:False", e.args))

Base.IteratorSize(_::GCPauseIter) = Base.SizeUnknown()
Base.IteratorEltype(_::GCPauseIter) = Base.HasEltype()
Base.eltype(_::GCPauseIter) = GCPause

event_pairs = Dict(
    :gc_begin => :gc_end,
    :ArenaPollBegin => :ArenaPollEnd,
    #:ArenaAccessBegin => :ArenaAccessEnd,
)

Base.iterate(g::GCPauseIter, state = (1, nothing, nothing)) =
    while true
        let (i, start, end_name) = state
            if i > length(g.events)
                @assert isnothing(start) "gc_begin without gc_end"
                return nothing
            else
                let e = g.events[i]
                    if e.name == end_name
                        @assert !isnothing(start) "end without begin"
                        if is_no_work_was_done_event(e)
                            state = (i+1, nothing, nothing)
                        else
                            return (
                                GCPause(start, e.time - start),
                                (i+1, nothing, nothing),
                            )
                        end
                    elseif haskey(event_pairs, e.name)
                        @assert isnothing(start) "begin without end"
                        state = (i+1, e.time, event_pairs[e.name])
                    else
                        state = (i+1, start, end_name)
                    end
                end
            end
        end
    end

as_trace(events::Vector{Event}, label)::Trace =
    let es = sort(events, by = x -> x.time)
        start = es[1].time
        duration = es[end].time - start
        rel = map(e -> Event(e.time - start, e.name, e.args), es)
        Trace(duration, collect(GCPauseIter(rel)), label)
    end

gc_pause_end(p::GCPause) = p.start + p.duration

gc_pause_intersection(p::GCPause, slice) =
    let (from, to) = slice
        (a, b) = (max(p.start, from), min(gc_pause_end(p), to))
        max(b-a, 0)
    end

gc_time(trace, slice) =
    let f(p) = gc_pause_intersection(p, slice)
        sum(map(f, trace.pauses))
    end

subplot_kwargs = (
    titlefontsize = 12,
    #titlealign = :left,
    leftmargin = (13, :mm),
    rightmargin = (10, :mm),
    bottommargin = (10, :mm),
    #topmargin = (-2, :mm),
)

plot_distribution(trace) =
    let pauses = map(p->p.duration, trace.pauses)
        title = @sprintf(
            "%s pause time; number: %d, sum: %0.3fs, \
                        min: %0.3f, median: %0.3f, max: %0.3f",
            trace.label,
            length(pauses),
            sum(pauses),
            minimum(pauses),
            #Statistics.quantile(pauses, 0.25),
            Statistics.median(pauses),
            #Statistics.quantile(pauses, 0.75),
            maximum(pauses)
        )
        Plots.histogram(
            pauses,
            bins = 0:0.001:(maximum(pauses)*1.1),
            label = trace.label,
            xguide = "Pause time buckets (sec)",
            yguide = "Number of pauses",
            title = title;
            margin = (12, :pt),
            subplot_kwargs...,
        )
    end


plot_traces(f, traces) = Plots.plot(
    map(f, traces)...;
    link = :x,
    layout = (:, 1),
    thickness_scaling = [0.8, 0.7, 0.5, 0.4, 0.3][length(traces)],
    #size = (600, 1000 - 150 * length(traces)),
)

plot_distributions(traces) = plot_traces(plot_distribution, traces)

mutator_utilization(trace, start, duration) =
    let mutator_time = duration - gc_time(trace, (start, start + duration))
        mutator_time / duration
    end

mmu(duration, trace, step = 0.001) =
    let mu(start) = mutator_utilization(trace, start, duration)
        minimum(mu, 0:step:(trace.duration-duration))
    end

plot_mmu(trace; resolution = 500) =
    let xs = logrange(0.005, trace.duration, length = resolution)
        ys = map(x -> mmu(x, trace), xs)
        Plots.plot(
            xs,
            ys,
            xscale = :log10,
            minorgrid = true,
            ylim = [0, 1],
            legend = :bottomright,
            label = trace.label,
            xguide = "Time slice (sec)",
            yguide = "MMU (1)",
            title = "MMU curve for " * trace.label;
            subplot_kwargs...,
        )
    end

plot_mmus(traces) = plot_traces(plot_mmu, traces)

# Vmetrics

struct VGoal
    gc::Float64
    duration::Float64
end

vmetrics(trace, goal; resolution = 500) =
    let xs = range(0, trace.duration, length = resolution)
        gctimes = map(x -> gc_time(trace, (x, x+goal.duration)), xs)
        vs = filter(gctime -> gctime > goal.gc, gctimes)
        length(vs) == 0 ? (0, 0, 0) :
        (
            length(vs) / length(xs),
            (Statistics.mean(vs) - goal.gc) / (goal.duration - goal.gc),
            (maximum(vs) - goal.gc) / (goal.duration - goal.gc),
        )
    end

plot_gc_overhead(trace, goal; resolution = 2000) =
    let xs = range(0, trace.duration, length = resolution)
        len = goal.duration
        ys = map(x->gc_time(trace, (x - len/2, x + len/2)), xs)
        vms = vmetrics(trace, goal)
        Plots.plot(
            xs,
            ys,
            ylim = [0, goal.duration],
            #seriestype = :sticks,
            label = trace.label,
            xguide = "Elapsed time (sec)",
            yguide = @sprintf("GC in %.03fs time slice (sec)", len),
            title = @sprintf(
                "GC overhead graph for %s with goal %.3fs | %.3fs
        (V%%: %.1f, avgV%%: %.1f, wV%%: %.1f)",
                trace.label,
                goal.gc,
                goal.duration,
                (100 .* vms)...
            );
            subplot_kwargs...,
        )
        Plots.plot!(xs, map(x->goal.gc, xs), label = "GC goal")
    end

plot_gc_overheads(traces, goal) =
    plot_traces(t -> plot_gc_overhead(t, goal), traces)

plot_cathedral(trace; resolution = 1000) =
    let xs = range(0, trace.duration, length = resolution)
        ys = logrange(0.0011, trace.duration*0.3, length = resolution)
        zs = [gc_time(trace, (x - y/2, x + y/2))/y for y in ys, x in xs]
        Plots.heatmap(
            xs,
            ys,
            zs,
            yscale = :log10,
            tick_direction = :out,
            minorgrid = true,
            xguide = "Elapsed time (sec)",
            yguide = "Time slice (sec)",
            title = "Cathedral graph for " * trace.label;
            subplot_kwargs...,
        )
    end

plot_cathedrals(traces) = plot_traces(plot_cathedral, traces)

pmu(duration, p, trace, resolution) =
    let mus = [
            mutator_utilization(trace, start, duration) for start in
            range(0, trace.duration-duration, length = resolution)
        ]
        Statistics.quantile(mus, (1 - p))
    end

plot_pmu(trace; xres = 500, sres = 5000) =
    let xs = logrange(0.005, trace.duration, length = xres)
        ps = [1.0, 0.99, 0.98, 0.9, 0.95, 0.90, 0.85]
        ys = [pmu(x, p, trace, sres) for x in xs, p in ps]
        Plots.plot(
            xs,
            ys,
            xscale = :log10,
            minorgrid = true,
            ylim = [0, 1],
            legend = :bottomright,
            label = map(string, reshape(ps, 1, :)),
            xguide = "Time slice (sec)",
            yguide = "PMU (1)",
            title = "PMU curve for range of percentiles for " *
                    trace.label;
            subplot_kwargs...,
        )
    end

plot_pmus(traces) = plot_traces(plot_pmu, traces)
