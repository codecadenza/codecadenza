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
package net.codecadenza.eclipse.generator.client.imp.eclipse.form.field;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.common.JavaFieldGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormPanel;
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
public class EclipseEnumComboFieldGenerator extends AbstractEclipseFieldGenerator {
	private final String enumTypeName;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public EclipseEnumComboFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.enumTypeName = field.getDTOAttribute().getDomainAttribute().getJavaType().getName();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		final String enumPackageName = field.getDTOAttribute().getDomainAttribute().getJavaType().getNamespace().toString();

		if (!field.isReadonly()) {
			formGenerator.importPackage("java.util");

			if (field.isVisible())
				formGenerator.importPackage(enumPackageName);
		}

		if (field.getDefaultValue() != null && !field.getDefaultValue().isEmpty())
			formGenerator.importPackage(enumPackageName);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addFieldDeclaration()
	 */
	@Override
	public void addFieldDeclaration() {
		if (!field.isVisible())
			return;

		if (!field.isReadonly()) {
			formGenerator.addPrivateField("Combo", field.getName()).create();

			final JavaFieldGenerator fieldGen = formGenerator.addPrivateField("Map<" + enumTypeName + ", String>",
					field.getName() + "TransMap");
			fieldGen.withDefaultValue("new EnumMap<>(" + enumTypeName + ".class)").withFinalModifier().create();
		}
		else
			formGenerator.addPrivateField("Text", field.getName()).create();
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
		final String getter = field.getDTOAttribute().getModelGetterName();
		final var enumeration = (JavaEnum) field.getDTOAttribute().getDomainAttribute().getJavaType();

		// Generate translations for all literals
		enumeration.getEnumerationValues().forEach(i18n::getI18N);

		if (field.isReadonly()) {
			final String checkStatement = getCheckFragment();

			if (!checkStatement.isEmpty())
				b.append("\n");

			// We just need the check-fragment if the field is read-only!
			b.append(checkStatement);

			b.append(field.getName() + ".setText(getTranslation(\"" + enumTypeName.toLowerCase() + "_\" + " + modelObjectName + ".");
			b.append(getter + ".name().toLowerCase()));\n");

			if (!checkStatement.isEmpty())
				b.append("\n");
		}
		else {
			b.append("\nfor(final " + enumTypeName + " item : " + enumTypeName + ".values())\n");
			b.append("{\n");
			b.append("final String translation = getTranslation(\"" + enumTypeName.toLowerCase());
			b.append("_\" + item.name().toLowerCase());\n");
			b.append(field.getName() + ".add(translation);\n");
			b.append(field.getName() + "TransMap.put(item, translation);\n");
			b.append("}\n\n");
			b.append("for(int i = 0; i < " + field.getName() + ".getItems().length; i++)\n");
			b.append("{\n");
			b.append("final String item = " + field.getName() + ".getItem(i);\n");
			b.append("final String translation = " + field.getName() + "TransMap.get(" + modelObjectName + "." + getter + ");\n\n");
			b.append("if(item.equals(translation))\n");
			b.append("{\n");
			b.append(field.getName() + ".select(i);\n");
			b.append("break;\n");
			b.append("}\n");
			b.append("}\n\n");
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getCreateInitializationFragment()
	 */
	@Override
	public String getCreateInitializationFragment() {
		if (!field.isVisible())
			return "";

		final var b = new StringBuilder();
		final var enumeration = (JavaEnum) field.getDTOAttribute().getDomainAttribute().getJavaType();

		// Generate translations for all literals
		enumeration.getEnumerationValues().forEach(i18n::getI18N);

		b.append("\nfor(final " + enumTypeName + " item : " + enumTypeName + ".values())\n");
		b.append("{\n");
		b.append("final String translation = getTranslation(\"" + enumTypeName.toLowerCase() + "_\" + item.name().toLowerCase());\n");
		b.append(field.getName() + ".add(translation);\n");
		b.append(field.getName() + "TransMap.put(item, translation);\n");
		b.append("}\n\n");

		if (field.getDefaultValue() != null && !field.getDefaultValue().isEmpty()) {
			b.append("\nfor(int i = 0; i < " + field.getName() + ".getItems().length; i++)\n");
			b.append("{\n");
			b.append("final String item = " + field.getName() + ".getItem(i);\n");
			b.append("final String translation = " + field.getName() + "TransMap.get(");
			b.append(enumTypeName + "." + field.getDefaultValue() + ");\n\n");
			b.append("if(item.equals(translation))\n");
			b.append("{\n");
			b.append(field.getName() + ".select(i);\n");
			b.append("break;\n");
			b.append("}\n");
			b.append("}\n\n");
		}
		else
			b.append(field.getName() + ".select(0);\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getSaveDataFragment(java.lang.String)
	 */
	@Override
	public String getSaveDataFragment(String objectName) {
		if (field.isReadonly())
			return "";

		final var b = new StringBuilder();
		final String setter = field.getDTOAttribute().getModelSetterName();
		final String enumName = field.getDTOAttribute().getDomainAttribute().getJavaType().getName();

		if (!field.isVisible()) {
			// Set the default values of invisible fields
			if (field.getDefaultValue() != null && !field.getDefaultValue().isEmpty())
				b.append(objectName + "." + setter + "(" + enumName + "." + field.getDefaultValue() + ");\n\n");
		}
		else {
			b.append("\nfor(final " + enumName + " item : " + enumName + ".values())\n");
			b.append("{\n");
			b.append("final String translation = " + field.getName() + "TransMap.get(item);\n\n");
			b.append("if(translation.equals(" + field.getName() + ".getItem(" + field.getName() + ".getSelectionIndex())))\n");
			b.append("{\n");
			b.append(objectName + "." + setter + "(item);\n");
			b.append("break;\n");
			b.append("}\n");
			b.append("}\n\n");
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.eclipse.form.field.AbstractEclipseFieldGenerator#
	 * getFieldDefinitionFragment()
	 */
	@Override
	public String getFieldDefinitionFragment() {
		final var b = new StringBuilder();
		final FormPanel panel = field.getPanel();

		if (field.isReadonly()) {
			b.append(field.getName() + " = new Text(" + panel.getName() + ", SWT.BORDER);\n");
			b.append(field.getName() + ".setEditable(false);\n");
			b.append(field.getName() + ".setBackground(Display.getCurrent().getSystemColor(SWT.COLOR_INFO_BACKGROUND));\n");
		}
		else
			b.append(field.getName() + " = new Combo(" + panel.getName() + ", SWT.READ_ONLY);\n");

		return b.toString();
	}

}
