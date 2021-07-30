# COVID-19-Mexico-Time-Series-Plots
 
Este repositorio contiene gráficas a nivel nacional y estatal del desarrollo de la epidemia del nuevo coronavirus SARS-CoV-2, casusante de COVID-19 en México. En fechas posteriores y si el tiempo y disponibilidad lo permiten, agregaré las tablas y bases de datos corregidos para su descarga y utilización. 

El respositorio está ordenado de la siguiente manera: la carpeta "Código", contiene el código en lenguaje R usado para la lectura, limpieza, codificación, segmentación, análisis y elaboración de todo el material gráfico y estadístico contenido en este repositorio. La segunda carpeta ("Graficas"), contiene una serie de subcarpetas en las cuales podrá consultar y descargar las diferentes gráficas elaboradas para este análisis. Las subcarpetas a su vez, están divididas en 11 contenedores que incluyen lo siguiente:

00_Resumen: Se muestran las gráficas con los datos nacionales y los resúmenes por estados de casos positivos y defunciones acumuladas, letalidad, casos activos, casos diarios, defunciones diarias, pruebas, porcentaje de positividad, tendencias nacionales y estatales, así como casos positivos y defunciones agrupadas por edad.

 01_Casos acumulados: Contiene los casos acumulados por estado y cuyo resultado en la base de datos abiertos de la Secretaría de Salud ha sido 'Positivo a SARS-CoV-2'.

 02_Defunciones acumuladas: Contiene las defunciones relacionadas a COVID-19 y cuyo resultado fue 'Positivo a SARS-CoV-2'.

 03_Letalidad: Contiene el cálculo de letalidad por semana y letalidad acumulada.

 04_Casos activos: Contiene el calculo de casos activos por estado cuyo resultado a SARS-CoV-2 es positivo y ha ocurrido en los últimos 14 días.

 05_Casos diarios: Contiene la incidencia de casos positivos mostrada por día y con un cálculo de media móvil de 7 días para los datos en ese periodo. 

 06_Defunciones diarias: Contiene la incidencia de defunciones asociadas a COVID-19 ordenados por fecha de defunciones y con un cálculo de media movil de 7 días para los datos de ese periodo. 

 07_Pruebas: Contiene el número de pruebas realizadas por día, tanto de antígenos como pruebas PCR y cuyo resultado es positivo y negativo.

 08_Positividad: Contiene el cálculo del porcentaje de positividad por semana y acumulado.

 09_Hospitalizaciones: Contiene la serie de tiempo de las hospitalizaciones por día en la Ciudad de México.

 10_Vacunación: Contiene un mapa didáctico que muestra el avance de la estrategia nacional de vacunación. 

El código usado es adaptado del código usado por @SEGASI y con agregaciones propias para usos y fines particulares. Gracias a la comunidad de R que hacen posible que más personas puedan acceder a estas nuevas formas de conocimiento, y podamos extender el uso de nuevas herramientas de análisis de datos. 
