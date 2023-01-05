export const reactSetterToStateImpl = ({ fst, snd }) => setState => f => () =>
    new Promise((res) => {
        setState((s_) => {
            let tpl = f(s_)
            let a = fst(tpl)
            let s = snd(tpl)
            res(a)
            return s
        })()
    })
