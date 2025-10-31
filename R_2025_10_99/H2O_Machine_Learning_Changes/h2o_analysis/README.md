# H2O-3 Changes Analysis

Este proyecto analiza los archivos de cambios (CHANGELOG) de H2O-3 para extraer información estructurada sobre cada versión liberada.

## 📁 Archivos

- `parse_h2o_changes.py` - Script principal para parsear los archivos de cambios
- `Changes.md` - Archivo de cambios recientes de H2O-3 (versiones >= 3.28.0.1)
- `Changes-prior-3.28.0.1.md` - Archivo de cambios antiguos de H2O-3 (versiones < 3.28.0.1)

## 📊 Datos Extraídos

El script extrae la siguiente información de cada versión:

1. **Número de versión** - Ej: 3.46.0.8
2. **Fecha** - Formato MM/DD/YYYY
3. **Número de nuevas funcionalidades** (New Features)
4. **Número de bugs corregidos**
5. **Número de mejoras** (Improvements)
6. **Número de cambios en documentación** (Docs)

## 🚀 Uso

```bash
python3 parse_h2o_changes.py
```

El script genera un archivo CSV en `/home/ubuntu/h2o_versions_data.csv`

## 📈 Estadísticas

- **Total de versiones analizadas**: 160
- **Rango de fechas**: 4/7/2016 - 10/8/2025
- **Total de cambios documentados**: 2,148
  - New Features: 346
  - Bugs: 890
  - Improvements: 405
  - Docs: 507

## 🔧 Características del Parser

El script es robusto y maneja:

- ✅ Múltiples formatos de Markdown (#### y HTML <h4>)
- ✅ Diferentes estilos de categorización
- ✅ Versiones sin información documentada
- ✅ Nombres de versiones con códigos adicionales
- ✅ Varios formatos de listas (markdown y HTML)

## 📦 Dependencias

```bash
pip install pandas
```

## 📄 Formato del CSV

```csv
Version,Date,New_Features,Bugs,Improvements,Docs
3.46.0.8,10/8/2025,0,5,0,2
3.46.0.7,3/27/2025,0,3,0,0
...
```

## 📝 Notas

- El CSV está ordenado por versión (más reciente primero)
- Las versiones sin información documentada muestran 0 en todas las categorías
- Todas las fechas están en formato MM/DD/YYYY
- El archivo está listo para ser importado en R para análisis posterior

## 🔗 Fuente

Los archivos de cambios provienen del repositorio oficial de H2O-3:
- https://github.com/h2oai/h2o-3/blob/master/Changes.md
- https://github.com/h2oai/h2o-3/blob/master/Changes-prior-3.28.0.1.md
