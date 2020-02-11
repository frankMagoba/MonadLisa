# MonadLisa

MonadLisa counts word occurrences in EPUB books.

This is a toy Haskell project to play with monads. Focus is on clarity and
simplicity rather than robustness, performance, or cleverness of any kind.

## Usage

Use Stack to build and run the program:

```
$ stack build
$ stack exec MonadLisa-exe -- [FILE...]
```

Example:

```
$ stack exec MonadLisa-exe Tolkien-LotR-Fellowship.epub Tolkien-LotR-Towers.epub Tolkien-LotR-King.epub
("THE",35219)
("AND",22423)
("OF",17369)
("TO",11268)
("A",9992)
("IN",9026)
("HE",8534)
("IT",6879)
("THAT",6765)
("WAS",6617)
("I",6415)
("THEY",5202)
("BUT",5108)
("YOU",4970)
("HIS",4907)
("SAID",4243)
("NOT",4181)
("FOR",4106)
("AS",3937)
("IS",3639)
...
```

## Licence

[Apache License 2.0](https://choosealicense.com/licenses/apache-2.0/)