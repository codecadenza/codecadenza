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
 * Generator for fields that provide the selection of multiple items
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EclipseListFieldGenerator extends AbstractEclipseFieldGenerator {
	private final DTOBean listDTO;
	private final BoundaryBean boundaryBean;
	private final BoundaryMethod method;
	private final String listName;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public EclipseListFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.listDTO = field.getDTOAttribute().getReferencedDTOBean();
		this.boundaryBean = project.getBoundaryByDomainObject(listDTO.getDomainObject());
		this.method = boundaryBean.getBoundaryMethodByReturnType(listDTO, BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER);
		this.listName = field.getDTOAttribute().getName();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (!field.isVisible())
			return;

		formGenerator.importPackage("net.codecadenza.runtime.richclient.eclipse.widget");

		if (project.isBoundaryMode())
			formGenerator.importPackage(listDTO.getNamespace().toString());
		else
			formGenerator.importPackage(listDTO.getDomainObject().getNamespace().toString());

		if (field.isReadonly())
			return;

		formGenerator.importPackage("java.util");

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

		formGenerator.addPrivateField("CheckboxDataGridComposite<" + listDTO.getModelClassName() + ">", field.getName()).create();
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

		if (field.isReadonly()) {
			b.append(field.getName() + ".setData(" + modelObjectName + "." + getter + ");\n");
			b.append(field.getName() + ".setCheckedElements(" + modelObjectName + "." + getter + ");\n");

			return b.toString();
		}

		b.append("\n");
		b.append("final Set<" + listDTO.getModelClassName() + "> " + listName + " = new HashSet<>(\n");

		new ServiceInvocationGenerator(method, listDTO, b).addInvocation(true, "null");

		b.append(");\n");
		b.append(listName + ".addAll(" + modelObjectName + "." + getter + ");\n\n");
		b.append(field.getName() + ".setData(" + listName + ");\n");
		b.append(field.getName() + ".setCheckedElements(" + modelObjectName + "." + getter + ");\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getCreateInitializationFragment()
	 */
	@Override
	public String getCreateInitializationFragment() {
		if (!field.isVisible() || field.isReadonly())
			return "";

		final var b = new StringBuilder();
		b.append("\n");
		b.append("final Collection<" + listDTO.getModelClassName() + "> " + listName + " = ");

		new ServiceInvocationGenerator(method, listDTO, b).addInvocation("null");

		b.append("\n");
		b.append(field.getName() + ".setData(" + listName + ");\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getSaveDataFragment(java.lang.String)
	 */
	@Override
	public String getSaveDataFragment(String dtoName) {
		final var b = new StringBuilder();
		final String getter = field.getDTOAttribute().getModelGetterName();

		if (!field.isVisible())
			return "";

		b.append("\n" + modelObjectName + "." + getter + ".clear();\n\n");
		b.append("for(final " + listDTO.getModelClassName() + " listElement : " + field.getName() + ".getCheckedElements())\n");
		b.append(modelObjectName + "." + getter + ".add(listElement);\n\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.eclipse.form.field.AbstractEclipseFieldGenerator#
	 * getFieldDefinitionFragment(boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		final var b = new StringBuilder();
		final FormPanel panel = field.getPanel();
		DTOBeanAttribute attr = listDTO.getDisplayAttribute();
		int fieldCount = 0;
		int columnSpan = 0;

		if (attr == null)
			attr = listDTO.getPKAttribute();

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

		String columnLabel = attr.getDomainAttribute().getLabel();
		columnLabel = columnLabel.substring(0, 1).toUpperCase() + columnLabel.substring(1);

		b.append(field.getName() + " = new CheckboxDataGridComposite<>(" + panel.getName() + ")\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.eclipse.widget.");
		b.append("__AbstractDataGridComposite#getCellText(java.lang.Object, int)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public String getCellText(" + listDTO.getModelClassName() + " element, int columnIndex)\n");
		b.append("{\n");
		b.append("return " + attr.getDomainAttribute().convertToString("element." + attr.getModelGetterName()) + ";\n");
		b.append("}\n");
		b.append("};\n\n");

		if (field.isReadonly())
			b.append(field.getName() + ".setEnabled(false);\n");

		b.append(field.getName() + ".addColumn(" + i18n.getI18N(attr, columnLabel) + ", 300);\n");
		b.append(addToolTipFragment());

		if (columnSpan > 0)
			b.append(field.getName() + ".setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, " + columnSpan + ", 1));\n\n");
		else
			b.append(field.getName() + ".setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));\n\n");

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
	protected String getFieldDefinitionFragment() {
		return null;
	}

}
