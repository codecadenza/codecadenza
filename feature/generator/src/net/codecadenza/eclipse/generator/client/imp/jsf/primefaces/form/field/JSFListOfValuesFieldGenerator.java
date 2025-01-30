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

import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.ValidationTypeEnumeration;

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
public class JSFListOfValuesFieldGenerator extends AbstractJSFFieldGenerator {
	private static final int DIALOG_WIDTH = 630;
	private static final int DIALOG_HEIGHT = 560;

	private final DTOBean listDTO;
	private final DTOBeanAttribute pkAttribute;
	private final String defaultItemName;
	private final FormTypeEnumeration formType;
	private final boolean addDefaultItem;
	private final DTOBeanAttribute displayAttribute;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public JSFListOfValuesFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.listDTO = field.getDTOAttribute().getReferencedDTOBean();
		this.pkAttribute = listDTO.getPKAttribute();
		this.displayAttribute = listDTO.getDisplayAttribute();
		this.defaultItemName = field.getDTOAttribute().getName() + "DefaultItem";
		this.formType = field.getPanel().getForm().getFormType();
		this.addDefaultItem = formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE
				|| (formType == FormTypeEnumeration.UPDATE && !field.isMandatory());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (!field.isVisible())
			return;

		if (project.isBoundaryMode())
			formGenerator.importPackage(listDTO.getNamespace().toString());
		else
			formGenerator.importPackage(listDTO.getDomainObject().getNamespace().toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getSaveDataFragment(java.lang.String)
	 */
	@Override
	public String getSaveDataFragment(String itemName) {
		final var b = new StringBuilder();

		if (!field.isVisible() || field.isReadonly() || project.isBoundaryMode() || field.isMandatory())
			return "";

		final JavaType pkType = listDTO.getPKAttribute().getDomainAttribute().getJavaType();

		b.append("\nif(" + modelObjectName + ".");
		b.append(field.getDTOAttribute().getModelGetterName() + "." + listDTO.getPKAttribute().getModelGetterName());

		if (pkType.isString())
			b.append(".isEmpty()");
		else
			b.append(" == " + listDTO.getPKAttribute().getDomainAttribute().getEmptyItemDefaultValue());

		b.append(")\n");
		b.append(modelObjectName + "." + field.getDTOAttribute().getModelSetterName() + "(null);\n\n");

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

		if (addDefaultItem) {
			b.append("\n");
			b.append("if(" + modelObjectName + "." + field.getDTOAttribute().getModelGetterName() + " == null)\n");
			b.append("{\n");
			b.append(addDefaultListItem(listDTO, defaultItemName));
			b.append(modelObjectName + "." + field.getDTOAttribute().getModelSetterName() + "(" + defaultItemName + ");\n");
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

		if (addDefaultItem) {
			b.append("\n");
			b.append(addDefaultListItem(listDTO, defaultItemName));
			b.append(modelObjectName + "." + field.getDTOAttribute().getModelSetterName() + "(" + defaultItemName + ");\n\n");
		}

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
		final String idFieldId = field.getName();
		var displayFieldId = "";
		final String lovName = field.getListOfValues().getName();
		int panelCount = 0;
		boolean placedInTabView = false;
		final int panelRowIndex = field.getPanel().getRowIndex();

		// Check if the field's panel resides in a tab view
		for (final FormPanel p : field.getPanel().getForm().getFormPanels())
			if (p.getRowIndex() == panelRowIndex)
				panelCount++;

		if (panelCount > 1)
			placedInTabView = true;

		if (displayAttribute != null)
			displayFieldId = idFieldId + "Disp";

		b.append(fillGridColumn(field, hasOneColumn, true));
		b.append(JSFGeneratorUtil.createFieldLabel(field, i18n));
		b.append("\t\t<p:inputText ");
		b.append("onfocus=\"document.getElementById('form:");

		if (placedInTabView)
			b.append("tabview" + panelRowIndex + ":");

		if (displayAttribute != null)
			b.append(displayFieldId);
		else
			b.append(idFieldId);

		b.append("').readOnly=true\" ");

		if (!field.isReadonly()) {
			b.append("ondblclick=\"openLOV('");
			b.append(lovName + "','#{request.contextPath}/lov/" + lovName + ".jsf'," + DIALOG_WIDTH + "," + DIALOG_HEIGHT);
			b.append(",'form:");

			if (displayAttribute == null) {
				if (placedInTabView)
					b.append("tabview" + panelRowIndex + ":");

				b.append(idFieldId + "'");
				b.append(",null); return false;\" ");
			}
			else {
				b.append(idFieldId + "'");
				b.append(",'form:");

				if (placedInTabView)
					b.append("tabview" + panelRowIndex + ":");

				b.append(displayFieldId + "')\" ");
			}
		}

		b.append("id=\"");

		if (displayAttribute != null)
			b.append(displayFieldId);
		else
			b.append(idFieldId);

		b.append("\" value=\"#{" + managedBeanName + "." + modelObjectName + "." + field.getDTOAttribute().getModelFieldName() + ".");

		if (displayAttribute != null)
			b.append(displayAttribute.getModelFieldName());
		else
			b.append(pkAttribute.getModelFieldName());

		b.append("}\">\n");

		if (!field.isReadonly() && field.isMandatory())
			b.append("\t\t\t<f:validateRequired/>\n");
		else if (!project.isBoundaryMode() && project.getValidationType() == ValidationTypeEnumeration.STANDARD) {
			// We must skip the validation if the facade mode and standard validation is selected!
			b.append("\t\t\t<f:validateBean disabled=\"true\"/>\n");
		}

		b.append("\t\t</p:inputText>\n");

		if (displayAttribute != null)
			b.append(addToolTipFragment(displayFieldId));
		else
			b.append(addToolTipFragment(idFieldId));

		b.append(fillGridColumn(field, hasOneColumn, false));
		b.append("\n");

		return b.toString();
	}

}
