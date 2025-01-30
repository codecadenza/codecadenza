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
package net.codecadenza.eclipse.generator.client.imp.javafx.form.field;

import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;

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
public class JavaFXListFieldGenerator extends AbstractJavaFXFieldGenerator {
	private final boolean searchable;
	private final BoundaryBean boundaryBean;
	private final BoundaryMethod method;
	private final String listName;
	private boolean containsOtherFields;
	private DTOBean listDTO;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 * @param searchable
	 */
	public JavaFXListFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator, boolean searchable) {
		super(field, formGenerator);

		this.searchable = searchable;
		this.listDTO = field.getDTOAttribute().getReferencedDTOBean();
		this.boundaryBean = project.getBoundaryByDomainObject(listDTO.getDomainObject());
		this.method = boundaryBean.getBoundaryMethodByReturnType(listDTO, BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER);
		this.listName = field.getDTOAttribute().getName();

		if (!field.isVisible())
			return;

		// Test if this panel contains other fields
		for (final FormField f : field.getPanel().getFields()) {
			if (f.equals(field))
				continue;

			if (!f.isVisible())
				continue;

			this.containsOtherFields = true;
			break;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.javafx.form.field.AbstractJavaFXFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (!field.isVisible())
			return;

		formGenerator.importPackage("java.util");

		if (project.isBoundaryMode())
			formGenerator.importPackage(listDTO.getNamespace().toString());
		else
			formGenerator.importPackage(listDTO.getDomainObject().getNamespace().toString());

		formGenerator.importPackage("net.codecadenza.runtime.richclient.javafx.control");
		formGenerator.importPackage("javafx.geometry");

		if (containsOtherFields) {
			formGenerator.importPackage("javafx.scene.control");
			formGenerator.importStatic("javafx.scene.layout.Region");
		}

		if (field.isReadonly())
			return;

		if (listDTO.getDomainObject().isMandated())
			formGenerator.addImports(securityHelper.getSecurityManagerImports());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addFieldDeclaration()
	 */
	@Override
	public void addFieldDeclaration() {
		if (!field.isVisible())
			return;

		formGenerator.addPrivateField("AbstractSelectionListPanel<" + listDTO.getModelClassName() + ">", field.getName()).create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getCreateInitializationFragment()
	 */
	@Override
	public String getCreateInitializationFragment() {
		if (!field.isVisible() || searchable || field.isReadonly())
			return "";

		final var b = new StringBuilder();
		b.append("\n");
		b.append("final Collection<" + listDTO.getModelClassName() + "> " + listName + " = ");

		new ServiceInvocationGenerator(method, listDTO, b).addInvocation("null");

		b.append("\n");
		b.append(field.getName() + ".setAvailableItems(" + listName + ");\n");

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
		final String getter = field.getDTOAttribute().getModelGetterName();

		b.append("\n");

		if (field.isReadonly()) {
			b.append("if(" + modelObjectName + "." + getter + " != null)\n");
			b.append(field.getName() + ".setSelectedItems(" + modelObjectName + "." + getter + ");\n\n");

			return b.toString();
		}

		if (!searchable) {
			b.append("final Collection<" + listDTO.getModelClassName() + "> " + listName + " = ");

			new ServiceInvocationGenerator(method, listDTO, b).addInvocation("null");

			b.append("\n");
			b.append("if(" + modelObjectName + "." + getter + " != null)\n");
			b.append("{\n");
			b.append(field.getName() + ".setSelectedItems(" + modelObjectName + "." + getter + ");\n");
			b.append(field.getName() + ".setAvailableItems(" + listName + ");\n");
			b.append("}\n");
			b.append("else\n");
			b.append(field.getName() + ".setAvailableItems(" + listName + ");\n\n");
		}
		else {
			b.append("if(" + modelObjectName + "." + getter + " != null)\n");
			b.append(field.getName() + ".setSelectedItems(" + modelObjectName + "." + getter + ");\n\n");
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getSaveDataFragment(java.lang.String)
	 */
	@Override
	public String getSaveDataFragment(String objectName) {
		if (!field.isVisible())
			return "";

		final var b = new StringBuilder();
		final String setter = field.getDTOAttribute().getModelSetterName();

		b.append(objectName + "." + setter + "(" + field.getName() + ".getSelectedItems());\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.javafx.form.field.AbstractJavaFXFieldGenerator#getFieldDefinitionFragment(
	 * boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		if (!field.isVisible())
			return "";

		final var b = new StringBuilder();
		final String modelClassName = listDTO.getModelClassName();
		final DTOBeanAttribute displayAttr = listDTO.getDisplayAttribute();
		boolean addTitle = false;

		// Test if a title should be added
		if (!containsOtherFields) {
			boolean addToTab = false;

			for (final FormPanel panel : form.getFormPanels())
				if (panel.getRowIndex() == field.getPanel().getRowIndex() && !panel.equals(field.getPanel())) {
					addToTab = true;
					break;
				}

			addTitle = !addToTab;
		}

		b.append("\n");
		b.append(field.getName() + " = new AbstractSelectionListPanel<>(");

		if (addTitle)
			b.append(i18n.getI18N(field));
		else
			b.append("null");

		b.append(", " + searchable);
		b.append(")\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.control.");
		b.append("AbstractSelectionListPanel#getItemText(java.lang.Object)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public String getItemText(" + modelClassName + " element)\n");
		b.append("{\n");

		if (displayAttr != null)
			b.append("return element." + displayAttr.getModelGetterName() + ";\n");
		else {
			final DTOBeanAttribute pkAttr = listDTO.getPKAttribute();
			final String getter = pkAttr.getModelGetterName();

			b.append("return " + pkAttr.getDomainAttribute().convertToString("element." + getter) + ";\n");
		}

		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.control.");
		b.append("AbstractSelectionListPanel#searchItems(java.lang.String)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public List<" + modelClassName + "> searchItems(String filter) throws Exception\n");
		b.append("{\n");

		if (!field.isReadonly()) {
			b.append("return ");

			if (searchable)
				new ServiceInvocationGenerator(method, listDTO, b).addInvocation("filter");
			else
				new ServiceInvocationGenerator(method, listDTO, b).addInvocation("null");
		}
		else
			b.append("return new ArrayList<>();\n");

		b.append("}\n");
		b.append("};\n\n");

		if (field.isReadonly())
			b.append(field.getName() + ".setDisable(true);\n");

		if (!containsOtherFields) {
			b.append(addToolTipFragment());
			b.append("\n");
			b.append(field.getPanel().getName() + ".getChildren().add(" + field.getName() + ");\n");
			b.append("VBox.setVgrow(" + field.getName() + ", Priority.ALWAYS);\n\n");
		}
		else
			b.append(super.getFieldDefinitionFragment(hasOneColumn));

		return b.toString();
	}

}
