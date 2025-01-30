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
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;

/**
 * <p>
 * Generator for fields that provide the search and the selection of multiple items
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularListFieldGenerator extends AbstractAngularItemSelectionFieldGenerator {
	/**
	 * Constructor
	 * @param field
	 * @param formatter
	 */
	public AngularListFieldGenerator(FormField field, AngularContentFormatter formatter) {
		super(field, formatter, field.getFieldType() == FormFieldTypeEnumeration.SEARCHABLE_LIST);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#getImports()
	 */
	@Override
	public Map<String, String> getImports() {
		// Return an empty map in order to skip the import of the @angular/forms Validators class!
		return Map.of();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#addBindingFragment()
	 */
	@Override
	public void addBindingFragment() {
		final var fieldDef = new StringBuilder();
		fieldDef.append("this.addControl('" + field.getDTOAttribute().getName() + "'");

		if (formType == FormTypeEnumeration.CREATE || formType == FormTypeEnumeration.ADD)
			fieldDef.append(", [], " + disabled + ", []");

		fieldDef.append(");");

		formatter.addLine(fieldDef.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#addFieldToTemplate()
	 */
	@Override
	public void addFieldToTemplate() {
		if (!field.isVisible())
			return;

		final var controlContainer = new StringBuilder();
		controlContainer.append("<cc-form-control-container [formGroup]=\"formGroup\" ");
		controlContainer.append("name=\"" + field.getDTOAttribute().getName() + "\" [span]=\"true\">");

		formatter.addLine(controlContainer.toString());
		formatter.increaseIndent();
		formatter.addLine(addControlToTemplate());
		formatter.decreaseIndent();
		formatter.addLine("</cc-form-control-container>");
		formatter.addBlankLine();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#addControlToTemplate()
	 */
	@Override
	protected String addControlToTemplate() {
		final var control = new StringBuilder();
		control.append("<cc-multi-selection-list labelFieldName=\"" + displayAttr.getName() + "\" ");
		control.append("[availableItems]=\"" + itemListName + "\" id=\"" + field.getName() + "\" ");

		if (filterItems) {
			if (displayAttr.getDomainAttribute().getJavaType().isIntegerOrLong())
				control.append("[minLength]=\"1\" ");

			control.append("[maxNumberOfItems]=\"50\" ");
			control.append("(filterInputChanged)=\"" + fetchItemsMethodName + "($event)\" ");
		}
		else
			control.append("[filterSourceItems]=\"false\" ");

		control.append("formControlName=\"" + field.getDTOAttribute().getName() + "\"");

		if (readonly || disabled)
			control.append(" [disabled]=\"true\"");

		control.append("></cc-multi-selection-list>");

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

		if (filterItems) {
			formatter.addBlockComment("Search " + listDTO.getDomainObject().getLabelPlural());
			formatter.addLine(fetchItemsMethodName + "(filter: string) {");
		}
		else {
			formatter.addBlockComment("Load all " + listDTO.getDomainObject().getLabelPlural());
			formatter.addLine(fetchItemsMethodName + "() {");
		}

		formatter.increaseIndent();
		formatter.addLine(invocationGenerator.createInvocation(getInvocationParameter()) + ".subscribe({");
		formatter.increaseIndent();
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
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularItemSelectionFieldGenerator#
	 * getInvocationParameter()
	 */
	@Override
	protected String getInvocationParameter() {
		if (filterItems)
			return "filter";

		return "'%'";
	}

}
