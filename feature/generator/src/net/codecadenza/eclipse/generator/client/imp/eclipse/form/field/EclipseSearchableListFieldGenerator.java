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

import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.eclipse.security.EclipseSecurityHelper;
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
public class EclipseSearchableListFieldGenerator extends AbstractEclipseFieldGenerator {
	private final DTOBean listDTO;
	private final BoundaryBean boundaryBean;
	private final BoundaryMethod method;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public EclipseSearchableListFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.listDTO = field.getDTOAttribute().getReferencedDTOBean();
		this.boundaryBean = project.getBoundaryByDomainObject(listDTO.getDomainObject());
		this.method = boundaryBean.getBoundaryMethodByReturnType(listDTO, BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
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

		formGenerator.importPackage("net.codecadenza.runtime.richclient.eclipse.widget");

		if (listDTO.getDomainObject().isMandated()) {
			final var securityHelper = new EclipseSecurityHelper(project);

			formGenerator.addImports(securityHelper.getSecurityManagerImports());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addFieldDeclaration()
	 */
	@Override
	public void addFieldDeclaration() {
		if (!field.isVisible())
			return;

		formGenerator.addPrivateField("DualDataSelectionListComposite<" + listDTO.getModelClassName() + ">", field.getName())
				.create();
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

		b.append(field.getName() + ".setSelectedItems(" + modelObjectName + "." + getter + ");\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getSaveDataFragment(java.lang.String)
	 */
	@Override
	public String getSaveDataFragment(String objectName) {
		final var b = new StringBuilder();
		final String setter = field.getDTOAttribute().getModelSetterName();

		if (!field.isVisible())
			return "";

		b.append(objectName + "." + setter + "(" + field.getName() + ".getSelectedItems());\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.eclipse.form.field.AbstractEclipseFieldGenerator#getFieldDefinitionFragment(
	 * boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		final var b = new StringBuilder();
		final FormPanel panel = field.getPanel();
		final DTOBeanAttribute displayAttr = listDTO.getDisplayAttribute();
		int fieldCount = 0;
		int columnSpan = 0;

		if (!field.isVisible())
			return "";

		// Calculate the number of visible form fields
		for (final FormField field : field.getPanel().getFields())
			if (field.isVisible())
				fieldCount++;

		final boolean span = (field.isSpanCols() && field.getColIndex() == 1) || fieldCount == 1;

		if (!span && field.getColIndex() == 2)
			fillEmptyColumns();

		// Determine the number of columns that the control will take up
		if (field.isSpanCols() && field.getColIndex() == 1)
			columnSpan = fieldCount > 1 ? 3 : 4;
		else if (fieldCount == 1)
			columnSpan = 4;

		// Add a field label if the panel contains further visible fields
		if (fieldCount > 1) {
			b.append("final var " + getFieldLabelName() + " = new Label(" + panel.getName() + ", SWT.NONE);\n");
			b.append(getFieldLabelName() + ".setText(" + i18n.getI18N(field, true) + ");\n\n");
		}

		b.append(field.getName() + " = new DualDataSelectionListComposite<>(" + panel.getName() + ", SWT.NONE)\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.eclipse.widget.");
		b.append("DualDataSelectionListComposite#getItemText(java.lang.Object)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public String getItemText(" + listDTO.getModelClassName() + " element)\n");
		b.append("{\n");

		if (displayAttr != null)
			b.append("return element." + displayAttr.getModelGetterName() + ";\n");
		else {
			final DTOBeanAttribute pkAttr = listDTO.getPKAttribute();

			b.append("return " + pkAttr.getDomainAttribute().convertToString("element." + pkAttr.getModelGetterName()) + ";\n");
		}

		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.eclipse.widget.");
		b.append("DualDataSelectionListComposite#searchItems(java.lang.String)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public Collection<" + listDTO.getModelClassName() + "> searchItems(String filter)\n");
		b.append("{\n");
		b.append("try\n");
		b.append("{\n");
		b.append("Display.getDefault().getActiveShell().setCursor(Display.getDefault().getSystemCursor(SWT.CURSOR_WAIT));\n\n");
		b.append("return ");

		new ServiceInvocationGenerator(method, listDTO, b).addInvocation("filter");

		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		formGenerator.addErrorLog(b, "Error while fetching data for list field '" + field.getName() + "'!", "e");

		b.append("\n");
		b.append("MessageDialog.openError(parentShell, " + i18n.getI18NMessage("msg_title_dialog_init", "Initialize dialog") + ", ");
		b.append(i18n.getI18NMessage("msg_err_fetching_data", "Error while fetching data! Message: ") + " + e.getMessage());\n");
		b.append("return Collections.emptyList();\n");
		b.append("}\n");
		b.append("finally\n");
		b.append("{\n");
		b.append("Display.getDefault().getActiveShell().setCursor(Display.getDefault().getSystemCursor(SWT.CURSOR_ARROW));\n");
		b.append("}\n");
		b.append("}\n");
		b.append("};\n\n");

		if (columnSpan > 0)
			b.append(field.getName() + ".setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, " + columnSpan + ", 1));\n");
		else
			b.append(field.getName() + ".setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));\n");

		b.append(addToolTipFragment());
		b.append("\n");

		if (!span && field.getColIndex() == 1)
			fillEmptyColumns();

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.eclipse.form.field.AbstractEclipseFieldGenerator#
	 * getFieldDefinitionFragment()
	 */
	@Override
	public String getFieldDefinitionFragment() {
		return null;
	}

}
