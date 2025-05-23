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
package net.codecadenza.eclipse.generator.client.imp.swing.util;

import java.util.Collection;
import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTable;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaEnum;

/**
 * <p>
 * Generator for common parts of views and grid panels of a Swing application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SwingCommonDataTableGenerator {
	private final AbstractJavaSourceGenerator parentGenerator;
	private final RichClientI18NGenerator i18n;
	private final String dtoName;
	private final Collection<TableColumnField> cols;
	private final BoundaryMethod boundaryMethod;
	private final FormTypeEnumeration formType;
	private final FormTable formTable;

	/**
	 * Constructor
	 * @param parentGenerator
	 * @param form
	 * @param i18n
	 */
	public SwingCommonDataTableGenerator(AbstractJavaSourceGenerator parentGenerator, Form form, RichClientI18NGenerator i18n) {
		this.parentGenerator = parentGenerator;
		this.i18n = i18n;
		this.dtoName = form.getDTO().getName();
		this.formTable = form.getViewFormPanel().getFormTable();
		this.cols = formTable.getFields();
		this.boundaryMethod = form.getBoundaryMethod();
		this.formType = form.getFormType();
	}

	/**
	 * Constructor
	 * @param parentGenerator
	 * @param formPanel
	 * @param i18n
	 */
	public SwingCommonDataTableGenerator(AbstractJavaSourceGenerator parentGenerator, FormPanel formPanel,
			RichClientI18NGenerator i18n) {
		this.parentGenerator = parentGenerator;
		this.i18n = i18n;
		this.dtoName = formPanel.getDTO().getName();
		this.formTable = formPanel.getFormTable();
		this.cols = formTable.getFields();
		this.boundaryMethod = formPanel.getBoundaryMethod();
		this.formType = null;
	}

	/**
	 * @return the generated content
	 */
	public String createFetchMethod() {
		final var b = new StringBuilder();
		final var declarationGenerator = new ServiceDeclarationGenerator(parentGenerator, boundaryMethod, b);
		final var invocationGenerator = new ServiceInvocationGenerator(boundaryMethod, b);

		b.append("/* (non-Javadoc)\n");

		if (formType == null)
			b.append(" * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#fetchData()\n");
		else if (formType == FormTypeEnumeration.LOV)
			b.append(" * @see net.codecadenza.runtime.richclient.swing.dialog.AbstractLOVDialog#fetchData(java.lang.String)\n");
		else
			b.append(" * @see net.codecadenza.runtime.richclient.swing.search.AbstractResultView#fetchData()\n");

		b.append(" */\n");
		b.append("@Override\n");
		b.append(parentGenerator.getAnnotationForGeneratedElement());

		if (formType != null && formType == FormTypeEnumeration.LOV)
			b.append("public List<" + dtoName + "> fetchData(String filter)\n");
		else
			b.append("public Collection<" + dtoName + "> fetchData()\n");

		b.append("{\n");
		b.append("// Get data from server\n");

		declarationGenerator.addLocalVariable();

		b.append("\ntry\n");
		b.append("{\n");
		b.append("return ");

		if (formType == null)
			invocationGenerator.addInvocation("id");
		else if (formType == FormTypeEnumeration.SIMPLE_VIEW || formType == FormTypeEnumeration.SEARCHABLE_VIEW)
			invocationGenerator.addInvocation("dataPanel.getSearchObj()");
		else if (formType == FormTypeEnumeration.LOV) {
			final var lovDTO = (DTOBean) boundaryMethod.getReturnType();
			new ServiceInvocationGenerator(boundaryMethod, lovDTO, b).addInvocation("filter");
		}

		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		parentGenerator.addErrorLog(b, "Error while fetching data!", "e");

		b.append("\n");

		if (formType == null)
			b.append("JOptionPane.showMessageDialog(this, ");
		else
			b.append("JOptionPane.showMessageDialog(dataPanel, ");

		b.append(i18n.getI18NMessage("msg_err_fetching_data", "Error while fetching data! Message: "));
		b.append(" + e.getMessage(), " + i18n.getI18NMessage("msg_title_data_fetch", "Fetch data"));
		b.append(", JOptionPane.ERROR_MESSAGE);\n\n");
		b.append("// We just return an empty list in case of an error!\n");
		b.append("return new ArrayList<>();\n");
		b.append("}\n");

		declarationGenerator.addCloseStatementInFinallyBlock();

		b.append("}\n\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	public String createCellExportText() {
		final var b = new StringBuilder();
		final var methodSignature = "String getCellExportText(" + dtoName + " element, int columnIndex)";

		b.append("/* (non-Javadoc)\n");

		if (formType == null)
			b.append(" * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel");
		else if (formType == FormTypeEnumeration.LOV)
			b.append(" * @see net.codecadenza.runtime.richclient.swing.dialog.AbstractLOVDialog");
		else
			b.append(" * @see net.codecadenza.runtime.richclient.swing.search.AbstractResultView");

		b.append("#getCellExportText(java.lang.Object, int)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(parentGenerator.getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		int i = 0;

		for (final TableColumnField col : cols) {
			if (!col.isVisible())
				continue;

			if (col.getFieldType() == TableColumnFieldTypeEnumeration.BOOLEAN) {
				final boolean addNullCheck = !col.getDTOAttribute().getSearchType().isPrimitive();
				final String getter = col.getDTOAttribute().getGetterName();

				b.append("if(columnIndex == " + i + ")\n");
				b.append("{\n");

				if (addNullCheck) {
					b.append("if(element." + getter + " != null)\n");
					b.append("{\n");
				}

				b.append("if(element." + getter + ")\n");
				b.append("return Boolean.TRUE.toString();\n");
				b.append("else\n");
				b.append("return Boolean.FALSE.toString();\n");

				if (addNullCheck)
					b.append("}\n");

				b.append("}\n\n");
			}

			i++;
		}

		b.append("return super.getCellExportText(element, columnIndex);\n");
		b.append("}\n\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	public String createCellValue() {
		final var b = new StringBuilder();
		final var methodSignature = "Comparable<?> getCellValue(" + dtoName + " element, int columnIndex)";

		b.append("/* (non-Javadoc)\n");

		if (formType == null)
			b.append(" * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel");
		else if (formType == FormTypeEnumeration.LOV)
			b.append(" * @see net.codecadenza.runtime.richclient.swing.dialog.AbstractLOVDialog");
		else
			b.append(" * @see net.codecadenza.runtime.richclient.swing.search.AbstractResultView");

		b.append("#getCellValue(java.lang.Object, int)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(parentGenerator.getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		int i = 0;

		b.append("switch(columnIndex)\n");
		b.append("{\n");

		for (final TableColumnField col : cols) {
			if (!col.isVisible())
				continue;

			final String getter = col.getDTOAttribute().getGetterName();

			b.append("case " + i + ":\n");

			if (col.getFieldType() == TableColumnFieldTypeEnumeration.ENUM) {
				final var javaEnum = (JavaEnum) col.getDTOAttribute().getDomainAttribute().getJavaType();

				// Generate translations for all literals
				javaEnum.getEnumerationValues().forEach(i18n::getI18N);

				b.append("return getTranslation(\"" + javaEnum.getName().toLowerCase());
				b.append("_\" + element." + getter + ".name().toLowerCase());\n");
			}
			else
				b.append("return element." + getter + ";\n");

			i++;
		}

		b.append("default:\n");
		b.append("return null;\n");
		b.append("}\n");
		b.append("}\n\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	public String createCellImage() {
		final var b = new StringBuilder();
		final var methodSignature = "Icon getCellImage(" + dtoName + " element, int columnIndex)";

		b.append("/* (non-Javadoc)\n");

		if (formType == null)
			b.append(" * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel");
		else if (formType == FormTypeEnumeration.LOV)
			b.append(" * @see net.codecadenza.runtime.richclient.swing.dialog.AbstractLOVDialog");
		else
			b.append(" * @see net.codecadenza.runtime.richclient.swing.search.AbstractResultView");

		b.append("#getCellImage(java.lang.Object, int)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(parentGenerator.getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		int i = 0;

		for (final TableColumnField col : cols) {
			if (!col.isVisible())
				continue;

			if (col.getFieldType() == TableColumnFieldTypeEnumeration.BOOLEAN) {
				final boolean addNullCheck = !col.getDTOAttribute().getSearchType().isPrimitive();
				final String getter = col.getDTOAttribute().getGetterName();

				b.append("if(columnIndex == " + i + ")\n");
				b.append("{\n");

				if (addNullCheck) {
					b.append("if(element." + getter + " != null)\n");
					b.append("{\n");
				}

				b.append("if(element." + getter + ")\n");
				b.append("return ImageLoader.getImage(ImageLoader.CHECKED);\n");
				b.append("else\n");
				b.append("return ImageLoader.getImage(ImageLoader.UNCHECKED);\n");

				if (addNullCheck)
					b.append("}\n");

				b.append("}\n\n");
			}

			i++;
		}

		b.append("return null;\n");
		b.append("}\n\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	public String createColumnDefinition() {
		final var b = new StringBuilder();
		final var methodSignature = "List<ColumnInfo> getColumnDefinition()";

		b.append("/* (non-Javadoc)\n");

		if (formType == null)
			b.append(" * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#getColumnDefinition()\n");
		else
			b.append(" * @see net.codecadenza.runtime.richclient.swing.dialog.AbstractLOVDialog#getColumnDefinition()\n");

		b.append(" */\n");
		b.append("@Override\n");
		b.append(parentGenerator.getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("final var colList = new ArrayList<ColumnInfo>();\n\n");

		int i = 0;

		// Add all visible table columns
		for (final TableColumnField col : cols) {
			if (!col.isVisible())
				continue;

			b.append("final var col" + i + " = new ColumnInfo(" + i + ", " + i18n.getI18N(col) + ", " + col.getWidth() + ");\n");
			b.append("colList.add(col" + i + ");\n\n");

			i++;
		}

		b.append("return colList;\n");
		b.append("}\n\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	public String createCellText() {
		final var b = new StringBuilder();
		final var methodSignature = "String getCellText(" + dtoName + " element, int columnIndex)";

		b.append("/* (non-Javadoc)\n");

		if (formType == null)
			b.append(" * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel");
		else if (formType == FormTypeEnumeration.LOV)
			b.append(" * @see net.codecadenza.runtime.richclient.swing.dialog.AbstractLOVDialog");
		else
			b.append(" * @see net.codecadenza.runtime.richclient.swing.search.AbstractResultView");

		b.append("#getCellText(java.lang.Object, int)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(parentGenerator.getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("switch(columnIndex)\n");
		b.append("{\n");

		int colIndex = 0;

		for (final TableColumnField col : cols) {
			if (!col.isVisible())
				continue;

			if (col.getFieldType() == TableColumnFieldTypeEnumeration.BOOLEAN) {
				// We should always return an empty string for boolean cells!
				colIndex++;
				continue;
			}

			final var getter = "element." + col.getDTOAttribute().getGetterName();
			final boolean addNullCheck = !col.getDTOAttribute().getSearchType().isPrimitive();

			b.append("case " + colIndex++ + ":\n");

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

	/**
	 * @return true if the getCellExportTextMethod() should be added
	 */
	public boolean addCellExportTextMethod() {
		return formTable.hasVisibleBooleanFields();
	}

}
