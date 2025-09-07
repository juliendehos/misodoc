# Page 2

## styling

text **strong** *italic*

## list

- [page 1 (internal link)](page1.md)

- [misodoc (external link)](https://github.com/juliendehos/misodoc/)

## ordered list

1. item1

1. item2

## image

![](velo.jpg)

## line break

line 1

* * *

line 2

## code 

foo `inline code` bar

```hs
mkComponent :: App Model Action
mkComponent = 
  (component mkModel updateModel viewModel)
    { initialAction = Just (ActionAskSummary "summary.md") }
```

## block

> foo
>
> bar

## table

| foo     | bar    |
|---------|--------|
| baz     | oof    |
| zab     | ooz    |

