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
package net.codecadenza.eclipse.generator.client.imp.angular.form.field;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.generator.client.imp.angular.common.TypeScriptFieldGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.util.AngularI18NGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
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
public class AngularEnumComboFieldGenerator extends AbstractAngularFieldGenerator {
	private final String itemListName;
	private final String initMethodName;
	private final JavaEnum javaEnum;

	/**
	 * Constructor
	 * @param field
	 * @param formatter
	 */
	public AngularEnumComboFieldGenerator(FormField field, AngularContentFormatter formatter) {
		super(field, formatter);

		this.itemListName = field.getDTOAttribute().getName() + "List";
		this.initMethodName = "init" + field.getDTOAttribute().getUpperCaseName() + "List";
		this.javaEnum = (JavaEnum) domainAttr.getJavaType();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#addControlToTemplate()
	 */
	@Override
	protected String addControlToTemplate() {
		final var control = new StringBuilder();
		control.append("<p-dropdown [options]=\"" + itemListName + "\" ");
		control.append("formControlName=\"" + field.getDTOAttribute().getName() + "\" ");
		control.append("[style]=\"{'width':'100%'}\" id=\"" + field.getName() + "\"");

		if (readonly || disabled)
			control.append(" [readonly]=\"true\"");

		control.append("></p-dropdown>");

		return control.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#getFields()
	 */
	@Override
	public List<TypeScriptFieldGenerator> getFields() {
		if (!field.isVisible())
			return Collections.emptyList();

		return Collections
				.singletonList(new TypeScriptFieldGenerator(itemListName + ": SelectItem[]", null, formatter).withDefaultValue("[]"));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#
	 * getFieldInitializationFragment()
	 */
	@Override
	public String getFieldInitializationFragment() {
		if (!field.isVisible())
			return null;

		return "this." + initMethodName + "();";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#getImports()
	 */
	@Override
	public Map<String, String> getImports() {
		final Map<String, String> imports = super.getImports();

		if (field.isVisible() || getInitialValue() != null)
			imports.put(javaEnum.getName(), "../../domain/" + javaEnum.getName().toLowerCase() + ".enum");

		if (field.isVisible())
			imports.put("SelectItem", "primeng/api");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#
	 * addMethods(net.codecadenza.eclipse.generator.client.imp.angular.other.AngularI18NGenerator)
	 */
	@Override
	public void addMethods(AngularI18NGenerator i18n) {
		if (!field.isVisible())
			return;

		formatter.addBlockComment("Add enumeration literals to form field '" + field.getName() + "'");
		formatter.addLine(initMethodName + "() {");
		formatter.increaseIndent();

		javaEnum.getEnumerationValues().forEach(enumLiteral -> {
			final var item = new StringBuilder();
			item.append("this." + itemListName + ".push({ label: " + i18n.getI18N(enumLiteral) + ", ");
			item.append("value: " + javaEnum.getName() + "[" + javaEnum.getName() + "." + enumLiteral.getName() + "] });");

			formatter.addLine(item.toString());
		});

		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#getJavaEnum()
	 */
	@Override
	public JavaEnum getJavaEnum() {
		if (!field.isVisible() && getInitialValue() == null)
			return null;

		return javaEnum;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#getInitialValue()
	 */
	@Override
	protected String getInitialValue() {
		String defaultLiteral = null;

		if (field.getDefaultValue() != null && !field.getDefaultValue().isEmpty())
			defaultLiteral = field.getDefaultValue();
		else if (field.isVisible() && (formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE))
			defaultLiteral = javaEnum.getEnumerationValues().stream().findFirst().map(EnumLiteral::getName).orElse(null);

		if (defaultLiteral != null)
			return javaEnum.getName() + "[" + javaEnum.getName() + "." + defaultLiteral + "]";

		return null;
	}

}
