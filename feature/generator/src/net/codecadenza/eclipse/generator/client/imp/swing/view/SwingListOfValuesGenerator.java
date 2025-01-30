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
package net.codecadenza.eclipse.generator.client.imp.swing.view;

import static net.codecadenza.eclipse.shared.Constants.APP_I18N_PROVIDER_CLASS;

import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.imp.swing.security.SwingSecurityHelper;
import net.codecadenza.eclipse.generator.client.imp.swing.util.SwingCommonDataTableGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormTable;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for list-of-values dialogs of a Swing application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SwingListOfValuesGenerator extends AbstractJavaSourceGenerator {
	private final Form form;
	private final DTOBean dto;
	private final Project project;
	private final RichClientI18NGenerator i18n;
	private final FormTable table;

	/**
	 * Constructor
	 * @param form
	 */
	public SwingListOfValuesGenerator(Form form) {
		super(form.getSourceFile());

		this.form = form;
		this.dto = form.getDTO();
		this.project = dto.getNamespace().getProject();
		this.i18n = new RichClientI18NGenerator(project);
		this.table = form.getViewFormPanel().getFormTable();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + APP_I18N_PROVIDER_CLASS);
		importPackage("java.awt");
		importPackage("javax.swing");
		importPackage("java.util");
		importClass("java.util.List");
		importPackage("net.codecadenza.runtime.richclient.swing.dialog");
		importPackage("net.codecadenza.runtime.richclient.swing.widget");
		importPackage("net.codecadenza.runtime.search");
		importPackage(dto.getNamespace().toString());

		if (dto.getDomainObject().isMandated()) {
			final var securityHelper = new SwingSecurityHelper(project);

			addImports(securityHelper.getSecurityManagerImports());
		}

		// Add imports due to field types
		for (final TableColumnField f : table.getFields())
			if (f.getFieldType() == TableColumnFieldTypeEnumeration.BOOLEAN)
				importPackage("net.codecadenza.runtime.richclient.swing.image");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public class ");
		b.append(form.getName());
		b.append(" extends AbstractLOVDialog<" + dto.getName() + ">");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Create the dialog\n");
		b.append(" * @param parent\n");
		b.append(" * @param filter\n");
		b.append(" * @param allowMultipleSelection\n");
		b.append(" * @param allowDeselection\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + form.getName());
		b.append("(Component parent, String filter, boolean allowMultipleSelection, boolean allowDeselection)\n");
		b.append("{\n");
		b.append("super(parent, filter, allowMultipleSelection, allowDeselection);\n");
		b.append("}\n\n");

		addConstructor(form.getName() + "(Component parent, String filter, boolean allowMultipleSelection, boolean allowDeselection)",
				b.toString());

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Create the dialog\n");
		b.append(" * @param parent\n");
		b.append(" * @param filter\n");
		b.append(" * @param allowMultipleSelection\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + form.getName() + "(Component parent, String filter, boolean allowMultipleSelection)\n");
		b.append("{\n");
		b.append("super(parent, filter, allowMultipleSelection);\n");
		b.append("}\n\n");

		addConstructor(form.getName() + "(Component parent, String filter, boolean allowMultipleSelection)", b.toString());

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Create the dialog\n");
		b.append(" * @param parent\n");
		b.append(" * @param filter\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + form.getName() + "(Component parent, String filter)\n");
		b.append("{\n");
		b.append("super(parent, filter);\n");
		b.append("}\n\n");

		addConstructor(form.getName() + "(Component parent, String filter)", b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final EList<TableColumnField> cols = table.getFields();
		final var tableGenerator = new SwingCommonDataTableGenerator(this, form, i18n);

		// Sort all table columns
		ECollections.sort(cols, (col1, col2) -> col1.getColIndex() - col2.getColIndex());

		TableColumnField colDisplay = cols.stream().filter(col -> col.getDTOAttribute().isLovReturn()).findFirst().orElse(null);

		if (colDisplay == null)
			colDisplay = cols.stream().filter(TableColumnField::isIdentifier).findFirst().orElse(null);

		if (colDisplay == null)
			throw new IllegalStateException("A display column for the list-of-values '" + form.getName() + "' has not been found!");

		var b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see java.awt.Dialog#getTitle\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public String getTitle()\n");
		b.append("{\n");
		b.append("return " + i18n.getI18N(form) + ";\n");
		b.append("}\n\n");

		addMethod("String getTitle()", b.toString());

		var methodSignature = "String getCellText(" + dto.getName() + " element, int columnIndex)";

		addMethod(methodSignature, tableGenerator.createCellText());

		methodSignature = "String setIdValue(" + dto.getName() + " element)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.swing.dialog.AbstractLOVDialog#setIdValue(java.lang.Object)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("if(element == null)\n");
		b.append("return null;\n\n");

		for (final TableColumnField col : cols)
			if (col.isIdentifier()) {
				final String getter = "element." + col.getDTOAttribute().getGetterName();

				b.append("return " + col.getDTOAttribute().getDomainAttribute().convertToString(getter) + ";\n");

				break;
			}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		final String displayGetter = colDisplay.getDTOAttribute().getGetterName();
		methodSignature = "String setDisplayValue(List<" + dto.getName() + "> elements)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.swing.dialog.");
		b.append("AbstractLOVDialog#setDisplayValue(java.util.List)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("final var result = new StringBuilder();\n\n");
		b.append("if(elements == null || elements.isEmpty())\n");
		b.append("return result.toString();\n\n");
		b.append("if(allowMultipleSelection && elements.size() > 1)\n");
		b.append("{\n");
		b.append("for(final " + dto.getName() + " elem : elements)\n");
		b.append("result.append(elem." + displayGetter + " + SearchService.TOKEN_DELIMITER_IN);\n\n");
		b.append("return result.toString();\n");
		b.append("}\n\n");
		b.append("final " + dto.getName() + " singleSel = elements.get(0);\n\n");
		b.append("if(singleSel == null)\n");
		b.append("return result.toString();\n\n");
		b.append("return \"\" + singleSel." + displayGetter + ";\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "List<ColumnInfo> getColumnDefinition()";

		addMethod(methodSignature, tableGenerator.createColumnDefinition());

		methodSignature = "Icon getCellImage(" + dto.getName() + " element, int columnIndex)";

		addMethod(methodSignature, tableGenerator.createCellImage());

		methodSignature = "Comparable<?> getCellValue(" + dto.getName() + " element, int columnIndex)";

		addMethod(methodSignature, tableGenerator.createCellValue());

		if (tableGenerator.addCellExportTextMethod()) {
			methodSignature = "String getCellExportText(" + dto.getName() + " element, int columnIndex)";

			addMethod(methodSignature, tableGenerator.createCellExportText());
		}

		methodSignature = "List<" + dto.getName() + "> fetchData(String filter)";

		addMethod(methodSignature, tableGenerator.createFetchMethod());

		// Add the method that implements getLogger()
		addGetLoggerMethod("net.codecadenza.runtime.richclient.swing.dialog.AbstractLOVDialog");

		i18n.save();
	}

}
