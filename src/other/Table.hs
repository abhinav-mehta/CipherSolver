module Table(Row,Table,Dim) where

type Row = [String]
type Table = [Row]
type Dim = [Int]

tableDim :: Table -> Dim
tableDim input = [ length input,  length (take 1 input)]
