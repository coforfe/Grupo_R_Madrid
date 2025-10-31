#!/usr/bin/env python3
"""
Script para parsear archivos de cambios de H2O-3 y extraer información estructurada
de cada versión liberada.
"""

import re
import pandas as pd
from datetime import datetime
from typing import Dict, List, Tuple, Optional

class H2OChangesParser:
    """Parser para archivos de cambios de H2O-3"""
    
    def __init__(self):
        self.versions_data = []
        
        # Patrones regex para identificar secciones
        self.version_patterns = [
            # Formato: ### 3.46.0.8 - 10/8/2025
            r'^###\s+([\w\.\-]+(?:\s+\([^)]+\))?)\s*[-–]\s*(\d{1,2}/\d{1,2}/\d{4})',
            # Formato: ### Yau (3.26.0.11) - 12/05/2019
            r'^###\s+([^-–]+?)\s*[-–]\s*(\d{1,2}/\d{1,2}/\d{4})',
        ]
        
        # Patrones para identificar categorías (soporta múltiples formatos)
        self.category_patterns = {
            'Bug': [
                r'^####\s*Bug(?:s)?(?:\s+Fix(?:es)?)?',
                r'^<h4>Bug(?:\s+Fix)?</h4>',
            ],
            'New Feature': [
                r'^####\s*New\s+Feature(?:s)?',
                r'^<h4>New\s+Feature(?:s)?</h4>',
            ],
            'Improvement': [
                r'^####\s*Improvement(?:s)?',
                r'^<h4>Improvement(?:s)?</h4>',
            ],
            'Docs': [
                r'^####\s*Docs?(?:\s+(?:Changes?|Updates?))?',
                r'^<h4>Docs?</h4>',
            ],
        }
        
    def parse_version_header(self, line: str) -> Optional[Tuple[str, str]]:
        """
        Extrae el número de versión y fecha de la línea de encabezado.
        
        Returns:
            Tupla (versión, fecha) o None si no se encuentra
        """
        for pattern in self.version_patterns:
            match = re.search(pattern, line, re.IGNORECASE)
            if match:
                version_raw = match.group(1).strip()
                date_str = match.group(2).strip()
                
                # Extraer solo el número de versión si hay texto adicional
                version_match = re.search(r'(\d+\.\d+\.\d+(?:\.\d+)?)', version_raw)
                if version_match:
                    version = version_match.group(1)
                else:
                    version = version_raw
                
                return version, date_str
        return None
    
    def identify_category(self, line: str) -> Optional[str]:
        """
        Identifica la categoría de una sección.
        
        Returns:
            Nombre de la categoría o None si no coincide
        """
        for category, patterns in self.category_patterns.items():
            for pattern in patterns:
                if re.search(pattern, line, re.IGNORECASE):
                    return category
        return None
    
    def count_items_in_section(self, lines: List[str], start_idx: int) -> int:
        """
        Cuenta el número de items en una sección específica.
        
        Args:
            lines: Lista de todas las líneas del archivo
            start_idx: Índice de inicio de la sección
            
        Returns:
            Número de items en la sección
        """
        count = 0
        i = start_idx + 1
        
        while i < len(lines):
            line = lines[i].strip()
            
            # Si encontramos otra sección o versión, terminamos
            if (line.startswith('###') or 
                line.startswith('####') or 
                line.startswith('<h3>') or 
                line.startswith('<h4>')):
                break
            
            # Contar items (varios formatos posibles)
            if line:
                # Formato markdown: - [[#16592]]
                if re.match(r'^[-*]\s*\[\[?#?\d+\]?\]', line):
                    count += 1
                # Formato HTML: <li>[<a href='...
                elif line.startswith('<li>'):
                    count += 1
                # Formato markdown simple: - Item
                elif re.match(r'^[-*]\s+\S', line):
                    count += 1
            
            i += 1
        
        return count
    
    def parse_file(self, filepath: str) -> None:
        """
        Parsea un archivo de cambios completo.
        
        Args:
            filepath: Ruta al archivo a parsear
        """
        print(f"\n📄 Parseando archivo: {filepath}")
        
        try:
            with open(filepath, 'r', encoding='utf-8') as f:
                lines = f.readlines()
        except Exception as e:
            print(f"❌ Error leyendo archivo {filepath}: {e}")
            return
        
        current_version = None
        current_date = None
        current_data = None
        versions_found = 0
        
        for i, line in enumerate(lines):
            line = line.strip()
            
            # Buscar encabezado de versión
            version_info = self.parse_version_header(line)
            if version_info:
                # Guardar versión anterior si existe
                if current_version and current_data:
                    self.versions_data.append(current_data)
                    versions_found += 1
                
                # Iniciar nueva versión
                current_version, current_date = version_info
                current_data = {
                    'Version': current_version,
                    'Date': current_date,
                    'New_Features': 0,
                    'Bugs': 0,
                    'Improvements': 0,
                    'Docs': 0
                }
                print(f"  ✓ Versión encontrada: {current_version} ({current_date})")
                continue
            
            # Buscar categorías solo si estamos dentro de una versión
            if current_version and current_data:
                category = self.identify_category(line)
                if category:
                    count = self.count_items_in_section(lines, i)
                    
                    if category == 'Bug':
                        current_data['Bugs'] = count
                    elif category == 'New Feature':
                        current_data['New_Features'] = count
                    elif category == 'Improvement':
                        current_data['Improvements'] = count
                    elif category == 'Docs':
                        current_data['Docs'] = count
                    
                    print(f"    - {category}: {count} items")
        
        # Guardar última versión
        if current_version and current_data:
            self.versions_data.append(current_data)
            versions_found += 1
        
        print(f"  ✅ Total de versiones extraídas: {versions_found}")
    
    def create_dataframe(self) -> pd.DataFrame:
        """
        Crea un DataFrame con los datos parseados.
        
        Returns:
            DataFrame con la información de todas las versiones
        """
        if not self.versions_data:
            print("⚠️ No se encontraron datos para crear el DataFrame")
            return pd.DataFrame()
        
        df = pd.DataFrame(self.versions_data)
        
        # Ordenar por versión (más reciente primero)
        df = df.sort_values('Version', ascending=False)
        
        return df
    
    def save_to_csv(self, output_path: str) -> None:
        """
        Guarda los datos en formato CSV.
        
        Args:
            output_path: Ruta del archivo CSV de salida
        """
        df = self.create_dataframe()
        
        if df.empty:
            print("❌ No hay datos para guardar")
            return
        
        try:
            df.to_csv(output_path, index=False)
            print(f"\n✅ Archivo CSV guardado exitosamente: {output_path}")
            print(f"   Total de versiones: {len(df)}")
            print(f"\n📊 Resumen estadístico:")
            print(f"   - Total New Features: {df['New_Features'].sum()}")
            print(f"   - Total Bugs: {df['Bugs'].sum()}")
            print(f"   - Total Improvements: {df['Improvements'].sum()}")
            print(f"   - Total Docs: {df['Docs'].sum()}")
            print(f"\n🔍 Vista previa (primeras 5 filas):")
            print(df.head().to_string())
        except Exception as e:
            print(f"❌ Error guardando CSV: {e}")


def main():
    """Función principal"""
    print("=" * 70)
    print("H2O-3 Changes Parser")
    print("=" * 70)
    
    # Rutas de archivos
    changes_file = "/home/ubuntu/h2o_analysis/Changes.md"
    changes_prior_file = "/home/ubuntu/h2o_analysis/Changes-prior-3.28.0.1.md"
    output_file = "/home/ubuntu/h2o_versions_data.csv"
    
    # Crear parser
    parser = H2OChangesParser()
    
    # Parsear ambos archivos
    parser.parse_file(changes_file)
    parser.parse_file(changes_prior_file)
    
    # Guardar resultados
    parser.save_to_csv(output_file)
    
    print("\n" + "=" * 70)
    print("✨ Proceso completado exitosamente")
    print("=" * 70)


if __name__ == "__main__":
    main()
