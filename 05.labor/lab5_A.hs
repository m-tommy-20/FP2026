-- # 5. labor

-- I. Írjuk meg a beépített splitAt, notElem, concat, repeat, replicate, cycle, iterate, any, all függvényeket.

-- II. Írjunk Haskell-függvényt, amely a foldl vagy a foldr függvényt alkalmazva

-- - implementálja a length, sum, elem, reverse, product, maximum, insert-sort, ++, map, filter függvényeket,
-- - meghatározza egy lista pozitív elemeinek összegét,
-- - egy lista páros elemeinek szorzatát,
-- - n-ig a négyzetszámokat.
-- - meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét: $$a_0 + x_0(a_1 + x_0(a_2 + x_0(a_3 + \ldots + x_0(a_{n-1}+ x_0 \cdot a_n))))$$

-- III.

-- - Írjunk egy Haskell-függvényt, amely egy String típusú listából meghatározza azokat a szavakat, amelyek karakterszáma a legkisebb. Például ha a lista a következő szavakat tartalmazza:  function class Float higher-order monad tuple variable Maybe recursion  akkor az eredmény-lista a következőkből áll: class Float monad tuple Maybe
-- - Írjunk egy talalat Haskell-függvényt, amely meghatározza azt a listát, amely a bemeneti listában megkeresi egy megadott elem előfordulási pozícióit.
--   Például a következő függvényhívások esetében az első az 5-ös előfordulási pozícióit, míg a második az e előfordulási pozícióinak listáját határozza meg.

--   ```haskell
--   > talalat 5 [3, 13, 5, 6, 7, 12, 5, 8, 5]
--   [2, 6, 8]
--   > talalat 'e' "Bigeri-vizeses"
--   [3,10,12]
--   ```
-- - Írjunk egy osszegT Haskell-függvényt, amely meghatározza egy (String, Int)értékpárokból álló lista esetében az értékpárok második elemeiből képzett összeget.
--   Például:

--   ```haskell
--   > ls = [("golya",120,"ms"),("fecske",85,"cj"),("cinege",132,"ms")]
--   > osszegT ls
--   337
--   ```

    ps ls = sum [t2 | (t1,t2,t3) <- ls]
    ps2 ls r = sum [t2 | (t1,t2,t3) <- ls1]
        where ls1 = filter (\(t1,t2,t3)-> t3 == r) ls
    main = do
        let ls = [("golya",120,"ms"),("fecske",85,"cj"),("cinege",132,"ms")]
        let result = ps ls
        let madarls = concatMap (<> " ") [t1 | (t1,t2,t3) <- ls]
        putStrLn $ madarls <> " populacio szama: " ++ show result 
        let ls1 = filter (\(t1,t2,t3)-> t3 == "ms") ls
        let madarls2 = concatMap (<> " ") [t1 | (t1,t2,t3) <- ls1]
        let result2 = ps2 ls "ms" 
        putStrLn $ madarls2 <> " " <> "ms" <> "-ben populacio szam: " ++ show result2


-- - Írjunk egy atlagTu Haskell-függvényt, amely egy kételemű, tuple elemtípusú lista esetében átlagértékeket számol a második elem szerepét betöltő listaelemeken. Az eredmény egy tuple elemtípusú lista legyen, amelynek kiíratása során a tuple-elemeket formázzuk, és külön sorba írjuk őket.
--   Például:

--   ```haskell
--   > :set +m
--   > ls = [("mari",[10, 6, 5.5, 8]), ("feri",[8.5, 9.5]),
--   | ("zsuzsa",[4.5, 7.9, 10]),("levi", [8.5, 9.5, 10, 7.5])]
--   > atlagTu ls
--   mari 7.375
--   feri 9.0
--   zsuzsa 7.466666666666666
--   levi 8.875
--   ```

