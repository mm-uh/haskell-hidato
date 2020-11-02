# Hidato

Este repositorio representa la respuesta a un proyecto de clases referente a la asignatura de `Programación Declarativa` de el curso de 4to año de la carrera `Ciencias de la Computación` pertenecientes a la `Universidad de la Habana`.

## Objetivos

El proyecto tiene 2 objetivos generales:
* Crear un programa en Haskell que dado un Hidato con algunas casillas anotadas sea capaz de darle solución.
* Crear un programa en Haskell que sea capaz de resolver Hidatos.

## Cómo usarlo 

Antes de poder usar nuestra aplicación es necesario tener instalado `cabal`, una herramienta para compilar aplicaciones hechas en Haskell. Una vez contamos con `cabal` debemos correr el siguiente comando para poder usar nuestra aplicación:
```
cabal build
```
Esto compilará los dos modulos principales que hacen uso de nuestra librería `HidatoLib` que es la que contiene la solución a los ejercicios popuestos en este trabajo. Estos módulos son:
* Generador
* Solucionador 

### Generador

La función del generador es a partir de una plantilla que cumple con ciertas características poder generar un tablero de Hidato que tiene una cantidad determinada de casillas sin ningún número para que después el solucionador pueda tomar este archivo y resolver el Hidato.

Se puede usar de la siguiente manera:
```
$ cabal run Generator -- --help
Hidato - Project of haskell                                                                          
                                                                                                   
Usage: Generator [--version] [--template VALUE] [--board_name VALUE] 
                 [--rotate INT]
  Hidato Generator haskell implementation
Available options:
  -h,--help                Show this help text
  --version                Show version
  --template VALUE         Selected template to generate Board
  --board_name VALUE       Name of generated value
  --rotate INT             Rotate entry 90 degrees n times
```
La plantilla por defecto se va a encontrar en `templates/Elipse.txt`, el nombre del tablero va a ser `board` y el valor de rotación sería 0.

### Solucionador

A partir de el tablero creado usando el generador, este módulo busca la solución del problema. La forma de usarlo es la siguiente:

```
$ cabal run Solver -- --help
Hidato - Project of haskell

Usage: Solver [--version] [--file VALUE]
  Hidato haskell implementation

Available options:
  -h,--help                Show this help text
  --version                Show version
  --file VALUE             file with generated hidato
```

## Otros

Para mas detalles se puede ver directamente la explicación del código usado y nuestras razones para hacer las cosas, en el informe [docs](./doc/Informe.pdf).
