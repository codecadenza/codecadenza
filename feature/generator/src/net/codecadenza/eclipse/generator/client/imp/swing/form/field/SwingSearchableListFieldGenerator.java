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
package net.codecadenza.eclipse.generator.client.imp.swing.form.field;

import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.swing.security.SwingSecurityHelper;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormField;
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
public class SwingSearchableListFieldGenerator extends AbstractSwingFieldGenerator {
	private final boolean searchable;
	private final BoundaryBean boundaryBean;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 * @param searchable
	 */
	public SwingSearchableListFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator, boolean searchable) {
		super(field, formGenerator);

		this.searchable = searchable;
		this.boundaryBean = project.getBoundaryByDomainObject(field.getDTOAttribute().getReferencedDTOBean().getDomainObject());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		final DTOBean listDTO = field.getDTOAttribute().getReferencedDTOBean();

		if (!field.isVisible())
			return;

		formGenerator.importClass("java.util.List");
		formGenerator.importClass("java.util.Collections");

		if (project.isBoundaryMode())
			formGenerator.importPackage(field.getDTOAttribute().getReferencedDTOBean().getNamespace().toString());
		else
			formGenerator.importPackage(field.getDTOAttribute().getReferencedDTOBean().getDomainObject().getNamespace().toString());

		formGenerator.importPackage("net.codecadenza.runtime.richclient.swing.widget");

		if (field.isReadonly())
			return;

		if (listDTO.getDomainObject().isMandated()) {
			final var securityHelper = new SwingSecurityHelper(project);

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

		final var typeName = "DualDataSelectionListPanel<" + field.getDTOAttribute().getReferencedDTOBean().getModelClassName() + ">";

		formGenerator.addPrivateField(typeName, field.getName()).create();
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
		if (!field.isVisible())
			return "";

		final var b = new StringBuilder();
		final String setter = field.getDTOAttribute().getModelSetterName();
		final String getter = field.getDTOAttribute().getModelGetterName();

		b.append("\n" + objectName + "." + getter + ".clear();\n\n");
		b.append("if(" + field.getName() + ".getSelectedItems() != null)\n");
		b.append(objectName + "." + setter + "(" + field.getName() + ".getSelectedItems());\n\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.swing.form.field.AbstractSwingFieldGenerator#getFieldDefinitionFragment(
	 * boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		if (!field.isVisible())
			return "";

		final var b = new StringBuilder();
		final String modelClassName = field.getDTOAttribute().getReferencedDTOBean().getModelClassName();
		final DTOBean dto = field.getDTOAttribute().getReferencedDTOBean();
		final DTOBeanAttribute displayAttr = dto.getDisplayAttribute();
		final BoundaryMethod method = boundaryBean.getBoundaryMethodByReturnType(dto, BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER);

		b.append("\n");
		b.append(field.getName() + " = new DualDataSelectionListPanel<>(" + searchable + ", ");

		if (field.isReadonly())
			b.append("true");
		else
			b.append("false");

		b.append(")\n");
		b.append("{\n");
		b.append("private static final long serialVersionUID = 1L;\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.swing.widget.");
		b.append("DualDataSelectionListPanel#getItemText(java.lang.Object)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public String getItemText(" + modelClassName + " element)\n");
		b.append("{\n");

		if (displayAttr != null)
			b.append("return element." + displayAttr.getModelGetterName() + ";\n");
		else {
			final DTOBeanAttribute pkAttr = dto.getPKAttribute();
			final var getter = "element." + pkAttr.getModelGetterName();

			b.append("return " + pkAttr.getDomainAttribute().convertToString(getter) + ";\n");
		}

		b.append("}\n\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.swing.widget.");
		b.append("DualDataSelectionListPanel#searchItems(java.lang.String)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public List<" + modelClassName + "> searchItems(String filter)\n");
		b.append("{\n");

		if (!field.isReadonly()) {
			b.append("try\n");
			b.append("{\n");
			b.append("setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));\n\n");

			final var invocationGenerator = new ServiceInvocationGenerator(method, dto, b);

			b.append("return ");

			if (searchable)
				invocationGenerator.addInvocation("filter");
			else
				invocationGenerator.addInvocation("null");

			b.append("}\n");
			b.append("catch (final Exception ex)\n");
			b.append("{\n");

			formGenerator.addErrorLog(b, "Error while fetching data for list field '" + field.getName() + "'!", "ex");

			b.append("\n");
			b.append("setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));\n");
			b.append("JOptionPane.showMessageDialog(" + field.getPanel().getForm().getName() + ".this, ");
			b.append(i18n.getI18NMessage("msg_err_fetching_data", "Error while fetching data! Message: "));
			b.append(" + ex.getMessage(), " + i18n.getI18NMessage("msg_title_dialog_init", "Initialize dialog"));
			b.append(", JOptionPane.WARNING_MESSAGE);\n");
			b.append("return Collections.emptyList();\n");
			b.append("}\n");
			b.append("finally\n");
			b.append("{\n");
			b.append("setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));\n");
			b.append("}\n");
		}
		else
			b.append("return Collections.emptyList();\n");

		b.append("}\n");
		b.append("};\n\n");
		b.append(addToolTipFragment(field.getName() + ".getTargetList()"));
		b.append("\n");
		b.append(field.getPanel().getName() + ".add(" + field.getName() + ", BorderLayout.CENTER);\n\n");

		return b.toString();
	}

}
