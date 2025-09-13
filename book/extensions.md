
# Extensions

MisoDoc also supports some MarkDown extensions.


## Autolinks

<https://github.com/juliendehos/misodoc>


## Emojis

If you write `:ramen:`, you get :ramen:.

Some emoji markups [here](https://gist.github.com/rxaviers/7360908).


## Tables

| foo     | bar               |
|---------|-------------------|
| :ramen: | oof               |
| rab     | [intro](intro.md) |


## Task lists

- [ ] todo
- [x] done


## Code highlighting

Using [highlightjs](https://highlightjs.org/).

```haskell
-- haskell
main :: IO ()
main = do
    input <- getLine
    putStrLn input
```

```cpp
// cpp
int main() {
    std::string input;
    std::getline(std::cin, input);
    std::cout << input << std::endl;
    return 0;
}
```

You'll probably have to [download your custom
build](https://highlightjs.org/download) of highlightjs.


## Math

MisoDoc can render math equations, using [KaTeX](https://katex.org/). 

If you write `$x = \sqrt{42}$`, you get $x = \sqrt{42}$.

You also have the display mode, for example:

```
$$x = {-b \pm \sqrt{b^2-4ac} \over 2a}$$
```

gives:

$$x = {-b \pm \sqrt{b^2-4ac} \over 2a}$$


## That's all folks! :notes:

