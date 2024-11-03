# Autor: Manuel Muñoz Márquez (manuel.munoz@uca.es)
# Licencia: GNU-GPL >= 3
# Proyecto: Proyecto R-UCA (http://knuth.uca.es/R)
#
#
# This file includes common functions of ui and server
# Don't edit this file unless you know what are you doing

### Actions definition
# Title of action menu
action.title <- function(language = 'en') {
    switch(language,
           es = 'Acción',
           'Action')
}
# Initialize common data for ui and server
actions.es <- c(add_column = 'Añadir columna', add_row = 'Añadir fila',
                download_data = 'Descargar datos', drop_column = 'Borrar columna', drop_row = 'Borrar fila',
                edit_cell = 'Editar casilla', delete_cell = 'Vaciar casilla', load_data = 'Cargar datos', load_example = 'Cargar ejemplos',
                rename_column = 'Renombrar columna', rename_row = 'Renombrar fila', renumerate_row = 'Renumerar filas', reset = 'Reiniciar',
                transform_into_factor = 'Transformar en factor')
actions.en <- c(add_column = 'Add column', add_row = 'Add row',
                data.file.type = 'Data file type', download_data = 'Download data', drop_column = 'Drop column', drop_row = 'Drop row',
                edit_cell = 'Edit cell', delete_cell = 'Empty cell', load_data = 'Load data', load_example = 'Load examples',
                rename_column = 'Rename column', rename_row = 'Rename row', renumerate_row = 'Renumerate rows', reset = 'Reset',
                transform_into_factor = 'Transform into factor')




text.es <- c(character = 'Carácter', column = 'Columna',
             decimal = 'Punto decimal', download_data = 'Descargar datos',
             example = 'Ejemplo', file = 'Fichero', go = 'Hacer', factor = 'Factor', main_menu = 'Menú principal de ejemplos', name = 'Nombre',
             numeric = 'Numérico', other_language = 'Engish version', other_url = '../PLRCR/', row = 'Fila',
             space = '<Espacio>', separator = "Separador",
             type = 'Tipo', file.type = 'Tipo de fichero',
             value ='Valor')

text.en <- c(character = 'Character', column = 'Column',
             decimal = 'Decimal point', download_data = 'Download data',
             example = 'Example', file = 'File', go = 'Go', factor = 'Factor', main_menu = 'Main examples menu', name = 'Name',
             numeric = 'Numeric', other_language = 'Versión española', other_url = '../PLRCR/', row = 'Row',
             space = '<Space>', separator = 'Separator',
             type = 'Type', file.type = 'File type',
             value = 'Value')

sfa.es <- c(sfa_actions = "Acciones AFS", 
            out_column = "Columna del output",
            prod_func = "Función de producción de frontera", cb = "Cobb-Douglas", tl = "Translogarítmica",
            ineff_term = "Distribución del término de ineficiencia", 
            hnormal = "Seminormal", exponential = "Exponencial", tnormal = "Normal truncada", gamma = "Gamma",
            data_type = "Tipo de datos",
            cross = "Datos transversales", panel = "Datos de panel/longitudinales",
            var1_panel = "Columna de la primera dimensión de los datos (i)", var2_panel = "Columna de la segunda dimensión de los datos (t)",
            confidence_level = "Nivel de significancia para los intervalos de confianza de la eficiencia",
            isLog = "Se ha tomado logaritmo de los datos",
            signific = "Niveles de significancia: **** - 0.001, *** - 0.01, ** - 0.05, * - 0.1",
            time_variant = "Modelo variante en el tiempo",
			
			error1 = "Carga datos primero", error2 = "Añade variables de input",
			par1 = "Coeficiente del parámetro", par2 = "Error estándar", par3 = "Ineficiencia media E[u]", par4 = "Eficiencia media E[exp(-u)]",
			par5 = "Parámetros del error compuesto", par6 = "Valor", par7 = "Parametro temporal"
)

sfa.en <- c(sfa_actions = "SFA Actions", 
            out_column = "Output Column",
            prod_func = "Production Frontier Function", cb = "Cobb-Douglas", tl = "Translogarithmic",
            ineff_term = "Inefficiency term distribution", 
            hnormal = "Half-Normal", exponential = "Exponential", tnormal = "Truncated normal", gamma = "Gamma",
            data_type = "Type of data",
            cross = "Cross-sectional data", panel = "Panel/Longitudinal data",
            var1_panel = "Column for the first dimension of the data (i)", var2_panel = "Column for the second dimension of the data (t)",
            confidence_level = "Confidence level for efficiency intervals",
            isLog = "Logarithm of the data has been taken",
            signific = "Significance levels: **** - 0.001, *** - 0.01, ** - 0.05, * - 0.1",
            time_variant = "Time-varying model",
			
			error1 = "Load some data first", error2 = "Add input variables",
			par1 = "Parameter Coefficient", par2 = "Standard Error", par3 = "Average inefficiency E[u]", par4 = "Average efficiency E[exp(-u)]",
			par5 = "Compound error parameters", par6 = "Value", par7 = "Time parameter"
)

actions <- actions.es
text <- text.es
sfa <- sfa.es

# Set default language
Language <- 'es'
#Language <- 'en'

# actions <- actions.en
# information <- information.en
# text <- text.en
# sfa <- sfa.en
