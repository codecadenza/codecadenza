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

import static net.codecadenza.eclipse.shared.Constants.EL_I18N_VAR;

import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.java.JavaType;

/**
 * <p>
 * Generator for element collection editors
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFElementCollectionEditorFieldGenerator extends AbstractJSFFieldGenerator {
	private final JavaType elementType;
	private final String typeName;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	public JSFElementCollectionEditorFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.elementType = field.getDTOAttribute().getDomainAttribute().getJavaType();
		this.typeName = "ElementCollectionEditorModel<" + elementType.getWrapperTypeName() + ">";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addImports()
	 */
	@Override
	public void addImports() {
		if (!field.isVisible())
			return;

		formGenerator.importClass("net.codecadenza.runtime.webclient.primefaces.model.ElementCollectionEditorModel");

		if (!field.isReadonly()) {
			formGenerator.importPackage("net.codecadenza.runtime.webclient.primefaces.util");
			formGenerator.importClass("jakarta.faces.application.FacesMessage");
		}

		if (elementType.getNamespace() != null)
			formGenerator.importPackage(elementType.getNamespace().getName());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addFieldDeclaration()
	 */
	@Override
	public void addFieldDeclaration() {
		if (!field.isVisible())
			return;

		formGenerator.addPrivateField(typeName, field.getName()).withTransientModifier().create();
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
		b.append(field.getName() + " = new ElementCollectionEditorModel<>(");
		b.append(JSFConstants.USER_SESSION_BEAN + ".getNumberFormat(), ");
		b.append(JSFConstants.USER_SESSION_BEAN + ".getDateTimeFormat(), ");
		b.append(JSFConstants.USER_SESSION_BEAN + ".getDateFormat(), ");
		b.append(elementType.getWrapperTypeName() + ".class);\n");
		b.append(field.getName() + ".setElements(" + modelObjectName + "." + getter + ");\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getCreateInitializationFragment()
	 */
	@Override
	public String getCreateInitializationFragment() {
		return getDefaultInitializationFragment();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.form.field.AbstractJSFFieldGenerator#addFieldMethods()
	 */
	@Override
	public void addFieldMethods() {
		if (!field.isVisible())
			return;

		final var comment = new StringBuilder("model of the element collection editor that is mapped to field '");
		comment.append(field.getDTOAttribute().getName());
		comment.append("'");

		formGenerator.addGetter(typeName, field.getName(), comment.toString());

		if (!field.isReadonly()) {
			final var b = new StringBuilder();
			final var methodSignature = "void addElementTo" + field.getDTOAttribute().getUpperCaseName() + "()";

			b.append("/**\n");
			b.append(" * Add a new element to the field '" + field.getDTOAttribute().getName() + "'\n");
			b.append(" */\n");
			b.append(formGenerator.getAnnotationForGeneratedElement());
			b.append("public " + methodSignature + "\n");
			b.append("{\n");
			b.append("try\n");
			b.append("{\n");
			b.append(field.getName() + ".addElement();\n");
			b.append("}");
			b.append("catch(Exception e)\n");
			b.append("{\n");
			b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_WARN, MSG_ADD_ELEMENT_FAILED, e);\n");
			b.append("}\n");
			b.append("}\n\n");

			formGenerator.addMethod(methodSignature, b.toString());
		}
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
		final String modelBinding = managedBeanName + "." + field.getName();
		final String tableId = field.getName() + "Table";

		if (!field.isReadonly()) {
			final String inputId = field.getName() + "Input";
			final String buttonId = field.getName() + "Add";
			final String addMethodName = "addElementTo" + field.getDTOAttribute().getUpperCaseName();

			b.append("\t\t<h:panelGrid columns=\"3\">\n");
			b.append("\t\t\t<h:outputLabel styleClass=\"label-field-mandatory\" ");
			b.append("value=\"#{" + EL_I18N_VAR + ".element_collection_editor_lbl_add}&#160;\"/>\n");
			b.append("\t\t\t<p:inputText autocomplete=\"off\" id=\"" + inputId + "\" ");
			b.append("size=\"40\" value=\"#{" + modelBinding + ".newElement}\">\n");
			b.append("\t\t\t\t<p:ajax event=\"keyup\" update=\"" + tableId + "\" ");
			b.append("listener=\"#{" + modelBinding + ".refreshElementsToBeDisplayed}\"/>\n");
			b.append("\t\t\t</p:inputText>\n");
			b.append("\t\t\t<p:commandButton value=\"#{" + EL_I18N_VAR + ".command_add}\" id=\"" + buttonId + "\" ");
			b.append("action=\"#{" + managedBeanName + "." + addMethodName + "}\"\n");
			b.append("\t\t\t\tupdate=\"" + tableId + ", msgBox\" process=\"@this " + inputId + "\"/>\n");
			b.append("\t\t</h:panelGrid>\n");
			b.append("\t\t<p/>\n");
			b.append("\t\t<h:outputLabel value=\"\" for=\"" + field.getName() + "\"/>\n\n");
		}

		b.append("\t\t<p:dataTable id=\"" + tableId + "\" var=\"item\"\n");
		b.append("\t\t\tselection=\"#{" + modelBinding + ".selectedRowElement}\"\n");
		b.append("\t\t\tvalue=\"#{" + modelBinding + ".rowElements}\" scrollable=\"false\"\n");
		b.append("\t\t\tpaginator=\"false\" resizableColumns=\"false\" selectionMode=\"single\" rowKey=\"#{item.rowKey}\">\n");

		if (!field.isReadonly())
			b.append("\t\t\t<p:ajax event=\"rowSelect\" listener=\"#{" + modelBinding + ".onRowSelect}\"/>\n\n");

		b.append("\t\t\t<p:column sortBy=\"#{item.value}\" width=\"100%\">\n");
		b.append("\t\t\t\t<f:facet name=\"header\">\n");
		b.append("\t\t\t\t\t<h:outputText value=\"#{" + EL_I18N_VAR + ".element_collection_editor_col_header_element}\"/>\n");
		b.append("\t\t\t\t</f:facet>\n");
		b.append("\t\t\t\t<h:outputText value=\"#{item.value}\"/>\n");
		b.append("\t\t\t</p:column>\n\n");
		b.append("\t\t\t<f:facet name=\"footer\">\n");
		b.append("\t\t\t\t<div style=\"white-space: nowrap; font-weight: bold;\">\n");
		b.append("\t\t\t\t\t#{i18n.element_collection_editor_lbl_no_of_elements} #{" + modelBinding + ".elements.size()}\n");
		b.append("\t\t\t\t</div>\n");
		b.append("\t\t\t</f:facet>\n");
		b.append("\t\t</p:dataTable>\n");

		if (!field.isReadonly()) {
			b.append("\n");
			b.append("\t\t<p:contextMenu for=\"" + tableId + "\">\n");
			b.append("\t\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".element_collection_editor_action_delete}\" ");
			b.append("id=\"" + field.getName() + "_mniDeleteElement\"\n");
			b.append("\t\t\t\taction=\"#{" + modelBinding + ".deleteElement}\" ");
			b.append("update=\"" + tableId + "\" process=\"@this\"/>\n");
			b.append("\t\t\t<p:menuitem value=\"#{" + EL_I18N_VAR + ".element_collection_editor_action_delete_all}\" ");
			b.append("id=\"" + field.getName() + "_mniDeleteAll\"\n");
			b.append("\t\t\t\taction=\"#{" + modelBinding + ".deleteAll}\" update=\"" + tableId + "\" ");
			b.append("process=\"@this\"/>\n");
			b.append("\t\t</p:contextMenu>\n");
		}

		b.append("\n");
		b.append(addToolTipFragment(tableId));
		b.append("\n");

		return b.toString();
	}

}
