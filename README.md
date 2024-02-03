# Abalone
[<img src="https://fr.wikipedia.org/wiki/Abalone_(jeu)#/media/Fichier:Abalone_standard.svg">](https://fr.wikipedia.org/wiki/Abalone_(jeu)#/media/Fichier:Abalone_standard.svg)

## Developpers 
1. Amadou Oury Diallo
2. Abounaim Elias
3. Jules Cotrez

## Building abalone

To build the project, type:

```
$ dune build
```

For continuous build, use

```
$ dune build --watch
```

instead.

## Running abalone

To play the game, type:

```
$ dune exec abalone
```

## Testing abalone

To test the project, type:

```
$ dune runtest
```

This can be combined with continuous build & test, using

```
$ dune runtest --watch
```

## Documentation  
[abalone rules](https://fr.wikipedia.org/wiki/Abalone_(jeu))