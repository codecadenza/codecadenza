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
package net.codecadenza.eclipse.generator.client.imp.vaadin.form.field;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.java.EnumLiteral;
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
public class VaadinEnumComboFieldGenerator extends AbstractVaadinFieldGenerator {
	private final String enumTypeName;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public VaadinEnumComboFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.enumTypeName = field.getDTOAttribute().getDomainAttribute().getJavaType().getName();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#getFieldTypeName()
	 */
	@Override
	protected String getFieldTypeName() {
		if (field.isReadonly())
			return "TextField";

		return "ComboBox<String>";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		final String enumPackageName = field.getDTOAttribute().getDomainAttribute().getJavaType().getNamespace().toString();

		formGenerator.importPackage(enumPackageName);

		if (field.isVisible()) {
			formGenerator.importPackage("net.codecadenza.runtime.webclient.vaadin.converter");
			formGenerator.importPackage("java.util");

			if (field.isReadonly())
				formGenerator.importPackage("com.vaadin.flow.component.textfield");
			else
				formGenerator.importPackage("com.vaadin.flow.component.combobox");
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#addFieldDeclaration()
	 */
	@Override
	public void addFieldDeclaration() {
		if (!field.isVisible())
			return;

		super.addFieldDeclaration();

		formGenerator.addPrivateField("Map<" + enumTypeName + ", String>", field.getName() + "TransMap").withFinalModifier()
				.withDefaultValue("new EnumMap<>(" + enumTypeName + ".class)").create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#getConversionFragment()
	 */
	@Override
	public String getConversionFragment() {
		final var b = new StringBuilder();

		if (!field.isVisible())
			return "";

		b.append(".withConverter(new StringToTranslationMapConverter<>(" + field.getName() + "TransMap, ");
		b.append(i18n.getLocaleFragment() + "))\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getDefaultInitializationFragment()
	 */
	@Override
	public String getDefaultInitializationFragment() {
		if (!field.isVisible())
			return "";

		final var b = new StringBuilder();
		final var enumeration = (JavaEnum) field.getDTOAttribute().getDomainAttribute().getJavaType();

		// Generate translations for all literals
		enumeration.getEnumerationValues().forEach(i18n::getI18N);

		b.append("for(final " + enumTypeName + " item : " + enumTypeName + ".values())\n");
		b.append(field.getName() + "TransMap.put(item, i18n.getTranslation(\"" + enumTypeName.toLowerCase());
		b.append("_\" + item.name().toLowerCase()));\n\n");

		if (!field.isReadonly())
			b.append(field.getName() + ".setItems(" + field.getName() + "TransMap.values());\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getCreateInitializationFragment()
	 */
	@Override
	public String getCreateInitializationFragment() {
		return getDefaultInitializationFragment();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#
	 * getDefaultValueInitialization()
	 */
	@Override
	public String getDefaultValueInitialization() {
		final var b = new StringBuilder();
		final var fullSetter = modelObjectName + "." + field.getDTOAttribute().getModelSetterName();
		final var enumeration = (JavaEnum) field.getDTOAttribute().getDomainAttribute().getJavaType();

		if (field.getDefaultValue() != null && !field.getDefaultValue().isEmpty())
			b.append(fullSetter + "(" + enumTypeName + "." + field.getDefaultValue() + ");\n");
		else {
			// If no default value is selected a random literal will be used!
			final EnumLiteral literal = enumeration.getEnumerationValues().stream().findFirst().orElse(null);

			if (literal != null)
				b.append(fullSetter + "(" + enumTypeName + "." + literal.getName() + ");\n");
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.vaadin.form.field.AbstractVaadinFieldGenerator#getFieldDefinitionFragment(
	 * boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		if (!field.isVisible())
			return "";

		final var b = new StringBuilder();
		final DomainAttribute attr = field.getDTOAttribute().getDomainAttribute();

		if (!field.isReadonly()) {
			b.append(field.getName() + " = new ComboBox<>();\n");
			b.append(field.getName() + ".setAllowCustomValue(false);\n");
		}
		else {
			b.append(field.getName() + " = new TextField();\n");

			if (attr.getUserComment() != null && !attr.getUserComment().isEmpty())
				b.append(field.getName() + ".setTitle(" + i18n.getI18N(attr) + ");\n");
		}

		b.append(field.getName() + ".setLabel(" + i18n.getI18N(field) + ");\n");
		b.append(field.getName() + ".setId(\"" + field.getName() + "\");\n");
		b.append(getFieldLayout(hasOneColumn));

		return b.toString();
	}

}
