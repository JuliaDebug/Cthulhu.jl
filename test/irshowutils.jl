function irshow_filename(fname, optimize, debuginfo, iswarn, hide_type_stable, inlining_costs, type_annotations)
    return "test_irshow-ground_truth/$fname-$(optimize ? :opt : :unopt)-$(debuginfo)-$(iswarn ? :warn : :nowarn)\
        -$(hide_type_stable ? :hide_type_stable : :show_type_stable)-$(inlining_costs ? :inlining_costs : :nocost)\
        -$(type_annotations ? :types : :notypes)"
end

function strip_base_linenums(s)
    s = replace(s, r"((boot|int|refvalue|refpointer)\.jl:)\d+" => s"\1")
    return s
end
