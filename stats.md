## Operations
- `Store`: stores 10k functions with 10 different types and up to 10 parameters
- `Get`:  performs 1k query for a function signature picked among the stored functions described in `Store` 

## Index Types
- FileBased: a naive implementation storing functions by appending them into a file
- FileBasedSorted: stores the functions in an ordered manner for more efficient queries

## Store uses 'insertion sort', early Get uses 'early return'

|Index Type \ Op(ms)| Store|   Get|
|-------------------|------|------|
|          FileBased|   17.|22402.|
|    FileBasedSorted|83485.|11315.|

## Store uses binary search

|Index Type \ Op(ms)|   Store|   Get|
|-------------------|--------|------|
|          FileBased|     12.|27983.|
|    FileBasedSorted|⚡17031.|16610.|

## Get uses binary search

|Index Type \ Op(ms)| Store|   Get|
|-------------------|------|------|
|          FileBased|    4.|25460.|
|    FileBasedSorted|17615.|⚡119.|

## Store uses C FFI `rename` instead of manual channel-based copy

|Index Type \ Op(ms)|   Store|   Get|
|-------------------|--------|------|
|          FileBased|     7. |26256.|
|    FileBasedSorted|⚡9948. |   92.|
