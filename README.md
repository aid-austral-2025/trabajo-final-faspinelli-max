# Trabajo Final

## Descripción

Se ha seleccionado para realizar el trabajo final de la materia **Análisis Inteligente de Datos** un dataset real que contiene información sobre las solicitudes de compras de los distintos sectores de un club y sus disciplinas.

Oiginalmente, la información esta en un servidor web en postgre. Para disponer de la información exporemos la información de una vista que contiene las cabeceras de las solicitudes y sus estados.

No se tendrán en cuenta para este trabajo, el detalle de esas solicitudes (los conceptos solicitados) ni los sectores por los que fue pasando la misma.

Las solicitudes deben ir pasando entre distintos sectores y asignándose posibles estados preconfigurados.

Existe un sector (Gerencia) que se ocupa de aprobar, modificar o rechazar la solicitud luego de que la misma haya sido cotizada por el sector compras . Si la misma es rechazada finaliza con ese estado, sino continua el circuito hacia los siguientes sectores.

En el dashboard, se podrá visualizar del lado izquierdo, una columna donde se definen los filtros para poder ir navegando sobre la información.

Habrá dos solapas con información, la general, que contendrá información por Periodo, de las solicitudes generadas, y los importes solicitados y aprobados. Esta información es muy relevante para el control de los importes que solicita cada sector.

Tanto desde el gráfico de Cantidad de solicitudes por sector como de su data table, si seleccionamos una fila, nos llevará automáticamente a la ventana de DETALLE, donde se podrá visualizar en una linea de tiempo las solicitudes que genero ese sector por periodo.

### 
