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
package net.codecadenza.eclipse.generator.client.imp.eclipse.view;

import static net.codecadenza.eclipse.shared.Constants.APP_I18N_PROVIDER_CLASS;

import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.eclipse.security.EclipseSecurityHelper;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormTable;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for list-of-values dialogs of an Eclipse RCP/RAP application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EclipseListOfValuesGenerator extends AbstractJavaSourceGenerator {
	private final Form form;
	private final Project project;
	private final RichClientI18NGenerator i18n;
	private final DTOBean dto;
	private final FormTable table;

	/**
	 * Constructor
	 * @param form
	 */
	public EclipseListOfValuesGenerator(Form form) {
		super(form.getSourceFile());

		this.form = form;
		this.project = form.getDomainObject().getNamespace().getProject();
		this.i18n = new RichClientI18NGenerator(project);
		this.dto = form.getDTO();
		this.table = form.getViewFormPanel().getFormTable();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + APP_I18N_PROVIDER_CLASS);
		importPackage("net.codecadenza.runtime.richclient.eclipse.widget");
		importPackage("net.codecadenza.runtime.richclient.eclipse.search");
		importClass("net.codecadenza.runtime.search.SearchService");
		importClass("org.eclipse.swt.widgets.Shell");
		importPackage("java.util");
		importClass("org.eclipse.swt.graphics.Image");
		importPackage(dto.getNamespace().toString());

		if (dto.getDomainObject().isMandated()) {
			final var securityHelper = new EclipseSecurityHelper(project);
			addImports(securityHelper.getSecurityManagerImports());
		}

		for (final TableColumnField f : table.getFields())
			if (f.getFieldType() == TableColumnFieldTypeEnumeration.BOOLEAN)
				importPackage("net.codecadenza.runtime.richclient.eclipse.image");
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
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		final var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Create the dialog\n");
		b.append(" * @param parentShell\n");
		b.append(" * @param filter\n");
		b.append(" * @param allowMultipleSelection\n");
		b.append(" * @param allowDeselection\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + form.getName() + "(Shell parentShell, String filter, ");
		b.append("boolean allowMultipleSelection, boolean allowDeselection)\n");
		b.append("{\n");
		b.append("super(parentShell, filter, allowMultipleSelection, allowDeselection);\n");
		b.append("}\n\n");

		addConstructor(
				form.getName() + "(Shell parentShell, String filter, boolean allowMultipleSelection, boolean allowDeselection)",
				b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		var b = new StringBuilder();
		int colIndex = 0;

		// Sort all table columns
		final EList<TableColumnField> cols = table.getFields();

		ECollections.sort(cols, (col1, col2) -> {
			final int value1 = col1.getColIndex();
			final int value2 = col2.getColIndex();

			if (value1 == value2)
				return 0;

			if (value1 > value2)
				return 1;

			return -1;
		});

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractLOVDialog#getTitle()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public String getTitle()\n");
		b.append("{\n");
		b.append("return " + i18n.getI18N(form) + ";\n");
		b.append("}\n\n");

		addMethod("String getTitle()", b.toString());

		var methodSignature = "String getColumnText(" + dto.getName() + " element, int columnIndex)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.eclipse.search.");
		b.append("__AbstractLOVDialog#getColumnText(java.lang.Object, int)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("switch(columnIndex)\n");
		b.append("{\n");

		for (final TableColumnField col : cols) {
			if (!col.isVisible())
				continue;

			if (col.getFieldType() == TableColumnFieldTypeEnumeration.BOOLEAN) {
				// We should always return an empty string for boolean cells!
				colIndex++;
				continue;
			}

			b.append("case " + colIndex++ + ":\n");

			final String getter = "element." + col.getDTOAttribute().getGetterName();
			final DomainAttribute domainAttribute = col.getDTOAttribute().getDomainAttribute();
			final boolean addNullCheck = !col.getDTOAttribute().getSearchType().isPrimitive();

			if (addNullCheck)
				b.append("if(" + getter + " != null)\n");

			b.append("return " + domainAttribute.convertToString(getter) + ";\n");

			if (addNullCheck)
				b.append("else\nreturn \"\";\n");
		}

		b.append("default:\n");
		b.append("return \"\";\n");
		b.append("}\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "String setIdValue(" + dto.getName() + " element)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.eclipse.search.");
		b.append("__AbstractLOVDialog#setIdValue(java.lang.Object)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("if(element == null)\n");
		b.append("return null;\n\n");

		for (final TableColumnField col : cols)
			if (col.isIdentifier()) {
				final String getter = col.getDTOAttribute().getGetterName();
				final DomainAttribute domainAttribute = col.getDTOAttribute().getDomainAttribute();

				b.append("return " + domainAttribute.convertToString("element." + getter) + ";\n");

				break;
			}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		TableColumnField colDisplay = cols.stream().filter(col -> col.getDTOAttribute().isLovReturn()).findFirst().orElse(null);

		if (colDisplay == null)
			colDisplay = cols.stream().filter(TableColumnField::isIdentifier).findFirst().orElse(null);

		if (colDisplay == null)
			throw new IllegalStateException("A display column for the list-of-values '" + form.getName() + "' has not been found!");

		final String displayGetter = colDisplay.getDTOAttribute().getGetterName();
		methodSignature = "String setDisplayValue(List<" + dto.getName() + "> elements)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.eclipse.search.");
		b.append("__AbstractLOVDialog#setDisplayValue(java.util.List)\n");
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

		methodSignature = "void initColumnsOfGrid(__AbstractDataGridComposite<" + dto.getName() + "> grid)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractLOVDialog#initColumnsOfGrid");
		b.append("(net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		for (final TableColumnField col : cols) {
			if (!col.isVisible())
				continue;

			if (col.getFieldType() == TableColumnFieldTypeEnumeration.ENUM) {
				final var javaEnum = (JavaEnum) col.getDTOAttribute().getDomainAttribute().getJavaType();

				// Generate translations for all literals
				javaEnum.getEnumerationValues().forEach(i18n::getI18N);
			}

			b.append("grid.addColumn(" + i18n.getI18N(col) + ", ColumnSortType.");

			if (col.getFieldType() == TableColumnFieldTypeEnumeration.STRING
					|| col.getFieldType() == TableColumnFieldTypeEnumeration.ENUM
					|| col.getFieldType() == TableColumnFieldTypeEnumeration.CHAR
					|| col.getFieldType() == TableColumnFieldTypeEnumeration.UUID_BINARY
					|| col.getFieldType() == TableColumnFieldTypeEnumeration.UUID_STRING)
				b.append("STRING, null");
			else if (col.getFieldType() == TableColumnFieldTypeEnumeration.BOOLEAN)
				b.append("BOOLEAN, null");
			else if (col.hasTemporalType()) {
				if (col.hasDateFormat())
					b.append("DATE, userFormat.getDateFormat()");
				else
					b.append("DATETIME, userFormat.getDateTimeFormat()");
			}
			else if (col.getFieldType() == TableColumnFieldTypeEnumeration.INTEGER
					|| col.getFieldType() == TableColumnFieldTypeEnumeration.LONG)
				b.append("INTEGER, null");
			else
				b.append("DECIMAL, userFormat.getDecimalFormat()");

			b.append(", " + col.getWidth() + ");\n");
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "Image getColumnImage(" + dto.getName() + " element, int columnIndex)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.eclipse.search.");
		b.append("__AbstractLOVDialog#getColumnImage(java.lang.Object, int)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		colIndex = 0;

		for (final TableColumnField col : cols) {
			if (!col.isVisible())
				continue;

			colIndex++;

			if (col.getFieldType() == TableColumnFieldTypeEnumeration.BOOLEAN) {
				final String getter = col.getDTOAttribute().getGetterName();
				final boolean addNullCheck = !col.getDTOAttribute().getSearchType().isPrimitive();

				b.append("if(columnIndex == " + (colIndex - 1) + ")\n");
				b.append("{\n");

				if (addNullCheck) {
					b.append("if(element." + getter + " != null)\n");
					b.append("{\n");
				}

				b.append("if(element." + getter + ")\n");
				b.append("return ImageCache.getImage(ImageCache.IMG_CHECKED);\n");
				b.append("else\n");
				b.append("return ImageCache.getImage(ImageCache.IMG_UNCHECKED);\n");

				if (addNullCheck)
					b.append("}\n");

				b.append("}\n\n");
			}
		}

		b.append("return null;\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "List<" + dto.getName() + "> fetchData(String filter)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.eclipse.search.");
		b.append("__AbstractLOVDialog#fetchData(java.lang.String)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		final var declarationGenerator = new ServiceDeclarationGenerator(this, form.getBoundaryMethod(), b);
		declarationGenerator.addLocalVariable();

		b.append("\n");

		if (declarationGenerator.needsCloseStatement()) {
			b.append("try\n");
			b.append("{\n");
		}

		b.append("return ");

		new ServiceInvocationGenerator(form.getBoundaryMethod(), dto, b).addInvocation("filter");

		if (declarationGenerator.needsCloseStatement()) {
			b.append("}\n");

			declarationGenerator.addCloseStatementInFinallyBlock();
		}

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		i18n.save();
	}

}
