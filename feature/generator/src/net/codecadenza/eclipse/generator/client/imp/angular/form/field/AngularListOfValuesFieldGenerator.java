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
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;

/**
 * <p>
 * Generator for list-of-values fields
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularListOfValuesFieldGenerator extends AbstractAngularFieldGenerator {
	private final String lovStatusName;
	private final String selectionHandlerMethodName;
	protected final DTOBean listDTO;

	/**
	 * Constructor
	 * @param field
	 * @param formatter
	 */
	public AngularListOfValuesFieldGenerator(FormField field, AngularContentFormatter formatter) {
		super(field, formatter);

		this.lovStatusName = "show" + field.getDTOAttribute().getUpperCaseName() + "Lov";
		this.selectionHandlerMethodName = "on" + field.getDTOAttribute().getUpperCaseName() + "Selected";
		this.listDTO = field.getDTOAttribute().getReferencedDTOBean();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#addControlToTemplate()
	 */
	@Override
	protected String addControlToTemplate() {
		final DTOBeanAttribute displayAttr = listDTO.getDisplayAttribute() == null ? listDTO.getPKAttribute()
				: listDTO.getDisplayAttribute();

		final var control = new StringBuilder();
		control.append("<cc-lov-input-field labelFieldName=\"" + displayAttr.getName() + "\" ");
		control.append("formControlName=\"" + field.getDTOAttribute().getName() + "\" id=\"" + field.getName() + "\" ");

		if (readonly || disabled)
			control.append("[readonly]=\"true\"");
		else
			control.append("(searchClicked)=\"" + lovStatusName + " = true\"");

		control.append("></cc-lov-input-field>");

		return control.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#getFields()
	 */
	@Override
	public List<TypeScriptFieldGenerator> getFields() {
		if (!field.isVisible() || readonly || disabled)
			return Collections.emptyList();

		return Collections.singletonList(new TypeScriptFieldGenerator(lovStatusName, null, formatter).withDefaultValue("false"));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#
	 * addMethods(net.codecadenza.eclipse.generator.client.imp.angular.other.AngularI18NGenerator)
	 */
	@Override
	public void addMethods(AngularI18NGenerator i18n) {
		if (!field.isVisible() || readonly || disabled)
			return;

		final DTOBean lovDTO = field.getListOfValues().getDTO();
		final DTOBeanAttribute displayAttr = listDTO.getDisplayAttribute();

		formatter.addBlockComment(
				"Handle the selection of a " + listDTO.getDomainObject().getLabel() + " item and close the respective dialog");
		formatter.addLine(selectionHandlerMethodName + "(selectedItem: " + listDTO.getName() + " | null) {");
		formatter.increaseIndent();

		if (!field.isMandatory()) {
			formatter.addLine("if (selectedItem) {");
			formatter.increaseIndent();
		}
		else {
			formatter.addLine("if (!selectedItem) {");
			formatter.increaseIndent();
			formatter.addLine("return;");
			formatter.decreaseIndent();
			formatter.addLine("}");
			formatter.addBlankLine();
		}

		// Copy all relevant values of the list-of-values DTO to the list DTO of the corresponding form field
		formatter.addLine("this.object." + field.getDTOAttribute().getName() + " = {");
		formatter.increaseIndent();

		final var mapping = new StringBuilder(formatter.getIndent());
		mapping.append(listDTO.getPKAttribute().getName() + ": selectedItem." + lovDTO.getPKAttribute().getName());

		for (final DTOBeanAttribute lovAttr : lovDTO.getAttributes())
			if (displayAttr != null && displayAttr.getDomainAttribute().equals(lovAttr.getDomainAttribute())
					&& lovAttr.getAssociationList().isEmpty()) {
				mapping.append(",\n" + formatter.getIndent() + displayAttr.getName() + ": selectedItem." + lovAttr.getName());
				break;
			}

		formatter.addContent(mapping.toString());
		formatter.addBlankLine();
		formatter.decreaseIndent();
		formatter.addLine("};");

		if (!field.isMandatory()) {
			formatter.decreaseIndent();
			formatter.addLine("} else {");
			formatter.increaseIndent();
			formatter.addLine("this.object." + field.getDTOAttribute().getName() + " = null;");
			formatter.decreaseIndent();
			formatter.addLine("}");
		}

		formatter.addBlankLine();
		formatter.addLine("this.formGroup.patchValue(this.object);");
		formatter.addLine("this." + lovStatusName + " = false;");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.form.field.AbstractAngularFieldGenerator#getImports()
	 */
	@Override
	public Map<String, String> getImports() {
		final Map<String, String> imports = super.getImports();

		if (field.isVisible() && !readonly && !disabled)
			imports.put(listDTO.getName(), "../../domain/" + listDTO.getName().toLowerCase() + ".interface");

		return imports;
	}

}
