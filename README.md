# Commodore 64 basic v2

![Static Badge](https://img.shields.io/badge/Estado%20-%20Terminado%20-%20green)

## Introducción
Trabajo practico de la materia lenguajes formales de la Facultad de Ingeniería Universidad de Buenos Aires
La idea detrás de este proyecto es simular lo mas posible a una maquina Commodore 64 basic V2, utilizando programación funcional, en este caso, para cumplir esto ultimo se utiliza Clojure.

## Tecnologías usadas
- Clojure

## Instalación
Para instalar Clojure hay que seguir los pasos del siguiente video:
https://www.youtube.com/watch?v=6uUynWkMDGM
## Uso
Después de clonar el repo o descargado el zip, es necesario abrir la terminal en la ruta del archivo.
Para poder usar la aplicación hay que utilizar el siguiente comando:
```
lein run
```
El proyecto trae distintos programas ya armados para poder usar ( Aquellos archivos terminados en .BAS ).
En caso de querer usarlos, ejecute el siguiente comando dentro del programa:
```
LOAD SINE.BAS
RUN
```
Se puede utilizar cualquiera de los archivos .BAS agregados en el proyecto.

Por ultimo, también trae un pequeño set de pruebas unitarias y para poder utilizarlas, ejecute el siguiente comando:
```
lein test
```
