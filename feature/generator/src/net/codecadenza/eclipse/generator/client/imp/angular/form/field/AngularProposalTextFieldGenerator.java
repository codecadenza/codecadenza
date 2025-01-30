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

import java.util.Map;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.generator.client.imp.angular.service.AngularServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.util.AngularI18NGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;

/**
 * <p>
 * Generator for fields that display proposal items after entering a filter text
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularProposalTextFieldGenerator extends AbstractAngularItemSelectionFieldGenerator {
	/**
	 * Constructor
	 * @param field
	 * @param formatter
	 */
	public AngularProposalTextFieldGenerator(FormField field, AngularContentFormatter formatter) {
		super(field, formatter, true);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#getImports()
	 */
	@Override
	public Map<String, String> getImports() {
		final Map<String, String> imports = super.getImports();

		if (field.isVisible())
			imports.put("AutoCompleteCompleteEvent", "primeng/autocomplete");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#addControlToTemplate()
	 */
	@Override
	protected String addControlToTemplate() {
		final var control = new StringBuilder();
		control.append("<p-autoComplete field=\"" + displayAttr.getName() + "\" ");
		control.append("formControlName=\"" + field.getDTOAttribute().getName() + "\" ");
		control.append("[forceSelection]=\"true\" ");
		control.append("[suggestions]=\"" + itemListName + "\" ");
		control.append("(completeMethod)=\"" + fetchItemsMethodName + "($event)\" ");
		control.append("[style]=\"{'width':'100%'}\" [inputStyle]=\"{'width':'100%'}\" ");
		control.append("id=\"" + field.getName() + "\"");

		if (displayAttr.getDomainAttribute().getJavaType().isString())
			control.append(" [minLength]=\"2\"");

		if (readonly || disabled)
			control.append(" [readonly]=\"true\"");

		control.append("></p-autoComplete>");

		return control.toString();
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

		final var invocationGenerator = new AngularServiceInvocationGenerator(method, listDTO);

		formatter.addBlockComment("Search " + listDTO.getDomainObject().getLabelPlural());
		formatter.addLine(fetchItemsMethodName + "(event: AutoCompleteCompleteEvent) {");
		formatter.increaseIndent();
		formatter.addLine(invocationGenerator.createInvocation(getInvocationParameter()) + ".subscribe({");
		formatter.increaseIndent();

		if (!field.isMandatory()) {
			final DTOBeanAttribute pkAttr = listDTO.getPKAttribute();
			final DTOBeanAttribute displayAttr = listDTO.getDisplayAttribute();
			final var pkDefaultValue = pkAttr.getDomainAttribute().getJavaType().isIntegerOrLong() ? "Number.MIN_VALUE" : "''";

			formatter.addLine("next: result => {");
			formatter.increaseIndent();
			formatter.addLine("this." + itemListName + " = result;");
			formatter.addBlankLine();
			formatter.addLine("const emptyItem = {");
			formatter.increaseIndent();
			formatter.addLine(pkAttr.getName() + " : " + pkDefaultValue + (displayAttr != null ? "," : ""));

			if (displayAttr != null)
				formatter.addLine(displayAttr.getName() + " : ''");

			formatter.decreaseIndent();
			formatter.addLine("};");
			formatter.addBlankLine();
			formatter.addLine("this." + itemListName + ".push(emptyItem);");
			formatter.decreaseIndent();
			formatter.addLine("},");
		}
		else
			formatter.addLine("next: result => this." + itemListName + " = result,");

		formatter.addLine("error: error => this.openErrorDialog(error, false)");
		formatter.decreaseIndent();
		formatter.addLine("});");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#
	 * getFieldValueReconversionFragment()
	 */
	@Override
	public String getFieldValueReconversionFragment() {
		if (!field.isVisible() || field.isMandatory() || disabled || readonly)
			return null;

		final DTOBeanAttribute pkAttr = listDTO.getPKAttribute();
		final var emptyValue = pkAttr.getDomainAttribute().getJavaType().isIntegerOrLong() ? "Number.MIN_VALUE" : "''";
		final var listAttr = "object." + field.getDTOAttribute().getName();

		final var convFragment = new StringBuilder();
		convFragment.append(listAttr + " = !" + listAttr + " || " + listAttr + "." + pkAttr.getName());
		convFragment.append(" === " + emptyValue + " ? null : " + listAttr + ";");

		return convFragment.toString();
	}

}
