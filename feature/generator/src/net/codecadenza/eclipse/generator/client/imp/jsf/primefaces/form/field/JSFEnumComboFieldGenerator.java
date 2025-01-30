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
package net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.form.field;

import static net.codecadenza.eclipse.shared.Constants.CONVERSION_SUFFIX;

import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.java.JavaEnum;

/**
 * <p>
 * Generator for combobox fields that are filled with enumeration literals
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFEnumComboFieldGenerator extends AbstractJSFFieldGenerator {
	private final String listMethodName;
	private final String enumName;
	private final String listName;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public JSFEnumComboFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.listName = field.getDTOAttribute().getName() + "List";
		this.listMethodName = "get" + field.getDTOAttribute().getUpperCaseName() + "List";
		this.enumName = field.getDTOAttribute().getDomainAttribute().getJavaType().getName();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		final String enumPackageName = field.getDTOAttribute().getDomainAttribute().getJavaType().getNamespace().toString();
		final FormTypeEnumeration formType = field.getPanel().getForm().getFormType();

		if (!field.isVisible() || field.isReadonly()) {
			if (formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE)
				formGenerator.importPackage(enumPackageName);
		}
		else {
			formGenerator.importPackage(enumPackageName);
			formGenerator.importPackage(project.getClientNamespace().toString());
			formGenerator.importPackage("jakarta.faces.model");
			formGenerator.importPackage("java.util");
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.form.field.AbstractJSFFieldGenerator#addFieldMethods()
	 */
	@Override
	public void addFieldMethods() {
		if (!field.isVisible() || field.isReadonly())
			return;

		final var b = new StringBuilder();
		final var javaEnum = (JavaEnum) field.getDTOAttribute().getDomainAttribute().getJavaType();

		// Generate translations for all literals
		javaEnum.getEnumerationValues().forEach(i18n::getI18N);

		b.append("/**\n");
		b.append(" * @return an array of selectable items\n");
		b.append(" */\n");
		b.append(formGenerator.getAnnotationForGeneratedElement());
		b.append("public SelectItem[] " + listMethodName + "()\n");
		b.append("{\n");
		b.append("final var items = new SelectItem[" + enumName + ".values().length];\n");
		b.append("int i = 0;\n\n");
		b.append("for(final " + enumName + " item : " + enumName + ".values())\n");
		b.append("items[i++] = new SelectItem(item, bundle.getString(\"");
		b.append(enumName.toLowerCase() + "_\" + item.name().toLowerCase()));\n\n");
		b.append("return items;\n");
		b.append("}\n\n");

		formGenerator.addMethod("SelectItem[] " + listMethodName + "()", b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getCreateInitializationFragment()
	 */
	@Override
	public String getCreateInitializationFragment() {
		final var b = new StringBuilder();
		final String setter = field.getDTOAttribute().getModelSetterName();

		// In any case we initialize an enumeration!
		if (field.getDefaultValue() != null && !field.getDefaultValue().isEmpty())
			b.append(modelObjectName + "." + setter + "(" + enumName + "." + field.getDefaultValue() + ");\n");
		else
			b.append(modelObjectName + "." + setter + "(" + enumName + ".values()[0]);\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getFieldDefinitionFragment(boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		if (!field.isVisible())
			return "";

		final var b = new StringBuilder();
		final var e = (JavaEnum) field.getDTOAttribute().getDomainAttribute().getJavaType();
		final String converterId = e.getLowerCaseName() + CONVERSION_SUFFIX;

		b.append(fillGridColumn(field, hasOneColumn, true));
		b.append(JSFGeneratorUtil.createFieldLabel(field, i18n));

		if (!field.isReadonly()) {
			b.append("\t\t<p:selectOneMenu ");
			b.append("converter=\"#{" + converterId + "}\" ");
			b.append("id=\"" + field.getName() + "\" value=\"#{" + managedBeanName + "." + modelObjectName + ".");
			b.append(field.getDTOAttribute().getModelFieldName() + "}\">\n");
			b.append("\t\t\t<f:selectItems value=\"#{" + managedBeanName + "." + listName + "}\"/>\n");
			b.append("\t\t</p:selectOneMenu>\n");
		}
		else {
			b.append("\t\t<p:inputText readonly=\"true\" converter=\"#{" + converterId + "}\" ");
			b.append("id=\"" + field.getName() + "\" value=\"#{" + managedBeanName + "." + modelObjectName + ".");
			b.append(field.getDTOAttribute().getModelFieldName() + "}\"/>\n");
		}

		b.append(addToolTipFragment(field.getName()));
		b.append(fillGridColumn(field, hasOneColumn, false));
		b.append("\n");

		return b.toString();
	}

}
