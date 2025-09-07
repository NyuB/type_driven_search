## Store sorted withh insertion sort, early return for search

|Index Type \ Op(ms)|       Store|         Get|
|-------------------|------------|------------|
|          FileBased|   17.019033|22402.179003|
|    FileBasedSorted|83485.322952|11315.924883|

## Store sorted with binary search

|Index Type \ Op(ms)|       Store|         Get|
|-------------------|------------|------------|
|          FileBased|   12.713194|27983.453035|
|    FileBasedSorted|17031.841040|16610.852003|

## Search uses binary search

|Index Type \ Op(ms)|        Store|          Get|
|-------------------|-------------|-------------|
|          FileBased| 4.8668384552|25460.2351189|
|    FileBasedSorted|17615.7889366|119.297027588|

## Store uses C FFI `rename` instead of manual channel-based copy
|Index Type \ Op(ms)|             Store|          Get|
|-------------------|------------------|-------------|
|          FileBased|     7.37690925598|26256.6931248|
|    FileBasedSorted| **9948.10795784**|92.6761627197|
