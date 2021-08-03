module CthulhuTestSandbox

function testf_revise()
    T = rand() > 0.5 ? Int64 : Float64
    sum(rand(T, 100))
end

export testf_revise

end
