/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.eclipse.tools.sqleditor.export.imp;

import java.util.List;
import net.codecadenza.eclipse.tools.sqleditor.export.IExportable;

/**
 * <p>
 * Export the result set in CSV format
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CSVExporter implements IExportable {
	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.tools.sqleditor.export.IExportable#export(java.lang.String[], java.util.List)
	 */
	@Override
	public String export(String[] colNames, List<List<String>> data) {
		final var output = new StringBuilder();

		for (final String colName : colNames) {
			output.append(colName);
			output.append(';');
		}

		output.append('\n');

		data.forEach(rowData -> {
			rowData.forEach(cellValue -> {
				if (cellValue.indexOf('\n') > -1)
					cellValue = cellValue.replace('\n', ' ');

				if (cellValue.indexOf('\r') > -1)
					cellValue = cellValue.replace('\r', ' ');

				if (cellValue.indexOf(';') > -1)
					cellValue = cellValue.replace(';', ' ');

				output.append(cellValue);
				output.append(';');
			});

			output.append('\n');
		});

		return output.toString();
	}

}
