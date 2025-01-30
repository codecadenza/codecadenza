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
package net.codecadenza.eclipse.generator.client.imp.javafx.util;

import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormTable;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.dto.DTOBean;

/**
 * <p>
 * Generator for common parts of views and grid panels of a JavaFX application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JavaFXCommonDataTableGenerator {
	private final AbstractJavaSourceGenerator parentGenerator;
	private final FormTable table;
	private final DTOBean dto;
	private final RichClientI18NGenerator i18n;

	/**
	 * Constructor
	 * @param parentGenerator
	 * @param table
	 * @param dto
	 * @param i18n
	 */
	public JavaFXCommonDataTableGenerator(AbstractJavaSourceGenerator parentGenerator, FormTable table, DTOBean dto,
			RichClientI18NGenerator i18n) {
		this.parentGenerator = parentGenerator;
		this.table = table;
		this.dto = dto;
		this.i18n = i18n;
	}

	/**
	 * @return the generated content
	 */
	public String initColumns() {
		final var b = new StringBuilder();
		final var methodSignature = "ObservableList<TableColumn<" + dto.getName() + ", String>> initColumns()";

		b.append("@Override\n");
		b.append(parentGenerator.getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("final ObservableList<TableColumn<" + dto.getName());
		b.append(", String>> columnList = FXCollections.observableArrayList();\n\n");

		int colIndex = 1;

		// Add all visible table columns
		for (final TableColumnField col : table.getFields()) {
			if (!col.isVisible())
				continue;

			final var colName = "col" + colIndex++;

			b.append("final var " + colName + " = new TableColumn<" + dto.getName() + ", String>();\n");
			b.append(colName + ".setText(" + i18n.getI18N(col) + ");\n");
			b.append(colName + ".setUserData(SearchFieldDataTypeEnum." + col.getFieldType().getName() + ");\n");
			b.append(colName + ".setCellValueFactory(param -> new SimpleStringProperty(getCellText(param.getValue(), ");
			b.append((colIndex - 2) + ")));\n");
			b.append(colName + ".setMinWidth(" + col.getWidth() + ");\n\n");
		}

		colIndex = 1;

		for (final TableColumnField col : table.getFields()) {
			if (!col.isVisible())
				continue;

			final var colName = "col" + colIndex++;

			b.append("columnList.add(" + colName + ");\n");
		}

		b.append("\n");
		b.append("return columnList;\n");
		b.append("}\n\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	public String getCellText() {
		final var b = new StringBuilder();
		final var methodSignature = "String getCellText(" + dto.getName() + " element, int columnIndex)";

		b.append("@Override\n");
		b.append(parentGenerator.getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("switch(columnIndex)\n");
		b.append("{\n");

		int colIndex = 0;

		for (final TableColumnField col : table.getFields()) {
			if (!col.isVisible())
				continue;

			final String getter = "element." + col.getDTOAttribute().getGetterName();

			b.append("case " + colIndex++ + ":\n");

			final boolean addNullCheck = !col.getDTOAttribute().getSearchType().isPrimitive();

			if (addNullCheck)
				b.append("if(" + getter + " != null)\n");

			b.append("return " + col.getDTOAttribute().getDomainAttribute().convertToString(getter) + ";\n");

			if (addNullCheck)
				b.append("else\nreturn \"\";\n");
		}

		b.append("default:\n");
		b.append("return \"\";\n");
		b.append("}\n");
		b.append("}\n\n");

		return b.toString();
	}

}
