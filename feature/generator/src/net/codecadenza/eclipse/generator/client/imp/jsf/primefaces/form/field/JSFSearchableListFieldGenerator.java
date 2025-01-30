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

import static net.codecadenza.eclipse.shared.Constants.CONVERSION_SUFFIX;
import static net.codecadenza.eclipse.shared.Constants.EL_I18N_VAR;

import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaType;

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
public class JSFSearchableListFieldGenerator extends AbstractJSFFieldGenerator {
	private boolean searchable;
	private final String filterAttrName;
	private final String filterMethodName;
	private final String modelSourceItems;
	private final String modelTargetItems;
	private final FormTypeEnumeration formType;
	private final DTOBean listDTO;
	private final String listName;
	private boolean addExistingItems = true;
	private final BoundaryBean boundaryBean;
	private final BoundaryMethod method;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 * @param searchable
	 */
	public JSFSearchableListFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator, boolean searchable) {
		super(field, formGenerator);

		this.searchable = searchable;
		this.listDTO = field.getDTOAttribute().getReferencedDTOBean();
		this.filterAttrName = field.getDTOAttribute().getName() + "FilterCriterion";
		this.filterMethodName = "filter" + field.getDTOAttribute().getUpperCaseName();
		this.modelSourceItems = field.getDTOAttribute().getName() + "SourceList";
		this.modelTargetItems = field.getDTOAttribute().getName() + "TargetList";
		this.formType = field.getPanel().getForm().getFormType();
		this.listName = field.getDTOAttribute().getName() + "List";
		this.boundaryBean = project.getBoundaryByDomainObject(listDTO.getDomainObject());
		this.method = boundaryBean.getBoundaryMethodByReturnType(listDTO, BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER);

		// If the field is read-only it doesn't make sense to search for items!
		if (field.isReadonly())
			this.searchable = false;

		if (formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE)
			this.addExistingItems = false;
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
		formGenerator.importClass("org.primefaces.model.DualListModel");

		if (project.isBoundaryMode())
			formGenerator.importPackage(listDTO.getNamespace().toString());
		else
			formGenerator.importPackage(listDTO.getDomainObject().getNamespace().toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#addFieldDeclaration()
	 */
	@Override
	public void addFieldDeclaration() {
		if (!field.isVisible())
			return;

		formGenerator.addPrivateField("DualListModel<" + listDTO.getModelClassName() + ">", listName).create();

		if (searchable)
			formGenerator.addPrivateField(JavaType.STRING, filterAttrName).create();
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

		if (!searchable) {
			if (!field.isReadonly()) {
				b.append("final List<" + listDTO.getModelClassName() + "> " + modelSourceItems + " = ");
				new ServiceInvocationGenerator(method, listDTO, b).addInvocation("null");
			}
			else
				b.append("final var " + modelSourceItems + " = new ArrayList<" + listDTO.getModelClassName() + ">();\n");

			if (addExistingItems) {
				b.append("final ArrayList<" + listDTO.getModelClassName() + "> " + modelTargetItems);
				b.append(" = new ArrayList<>(" + modelObjectName + "." + getter + ");\n");

				if (!field.isReadonly())
					b.append("\n" + modelSourceItems + ".removeAll(" + modelTargetItems + ");\n");
			}
			else
				b.append("final var " + modelTargetItems + " = new ArrayList<" + listDTO.getModelClassName() + ">();\n");
		}
		else {
			b.append("final var " + modelSourceItems + " = new ArrayList<" + listDTO.getModelClassName() + ">();\n");

			if (addExistingItems) {
				b.append("final ArrayList<" + listDTO.getModelClassName() + "> " + modelTargetItems);
				b.append(" = new ArrayList<>(" + modelObjectName + "." + getter + ");\n");
			}
			else
				b.append("final var " + modelTargetItems + " = new ArrayList<" + listDTO.getModelClassName() + ">();\n");
		}

		b.append("\n");
		b.append(listName + " = new DualListModel<>(" + modelSourceItems + ", " + modelTargetItems + ");\n\n");

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
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getSaveDataFragment(java.lang.String)
	 */
	@Override
	public String getSaveDataFragment(String itemName) {
		final var b = new StringBuilder();
		final String setter = field.getDTOAttribute().getModelSetterName();

		if (!field.isVisible())
			return "";

		b.append(itemName + "." + setter + "(" + listName + ".getTarget());\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.form.field.AbstractJSFFieldGenerator#addFieldMethods()
	 */
	@Override
	public void addFieldMethods() {
		if (!field.isVisible())
			return;

		formGenerator.addGetterAndSetter("DualListModel<" + listDTO.getModelClassName() + ">", listName, "the item list");

		if (searchable) {
			formGenerator.addGetterAndSetter(JavaType.STRING, filterAttrName, "the filter criterion");

			final var b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Perform filter operation\n");
			b.append(" */\n");
			b.append(formGenerator.getAnnotationForGeneratedElement());
			b.append("public void " + filterMethodName + "()\n");
			b.append("{\n");
			b.append(listName + " = new DualListModel<>(Collections.emptyList(), " + listName + ".getTarget());\n\n");
			b.append("if(" + filterAttrName + " == null || " + filterAttrName + ".isEmpty())\n");
			b.append("return;\n\n");
			b.append("try\n");
			b.append("{\n");
			b.append("final List<" + listDTO.getModelClassName() + "> " + modelSourceItems + " = ");

			new ServiceInvocationGenerator(method, listDTO, b).addInvocation(filterAttrName);

			b.append(modelSourceItems + ".removeAll(" + listName + ".getTarget());\n\n");
			b.append(listName + " = new DualListModel<>(" + modelSourceItems + ", " + listName + ".getTarget());\n");
			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			formGenerator.addErrorLog(b, "Error while fetching data for list field '" + field.getName() + "'!", "e");

			b.append("}\n");
			b.append("}\n\n");

			formGenerator.addMethod("void " + filterMethodName + "()", b.toString());
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
		final DTOBeanAttribute pkAttr = listDTO.getPKAttribute();
		final String converterName = JSFGeneratorUtil.createManagedBeanName(listDTO.getModelClassName() + CONVERSION_SUFFIX);
		DTOBeanAttribute displayAttr = listDTO.getDisplayAttribute();
		final var varName = field.getName() + "Item";

		// Note that this kind of field should be put on a separate panel!
		if (displayAttr == null)
			displayAttr = pkAttr;

		if (searchable) {
			final var inputId = field.getName() + "Filter";
			final var buttonId = field.getName() + "Button";
			final var groupName = field.getName() + "Group";

			b.append("\t\t<h:panelGroup id=\"" + groupName + "\">\n");
			b.append("\t\t<h:outputLabel/>\n");
			b.append("\t\t<h:panelGrid columns=\"3\">\n");
			b.append("\t\t\t<h:outputLabel styleClass=\"label-field-mandatory\" value=\"#{" + EL_I18N_VAR);
			b.append(".label_list_enter_filter}\" for=\"" + inputId + "\"/>\n");
			b.append("\t\t\t<p:inputText id=\"" + inputId + "\" value=\"#{" + managedBeanName + "." + filterAttrName + "}\">\n");
			b.append("\t\t\t</p:inputText>\n");
			b.append("\t\t\t<p:commandButton value=\"#{" + EL_I18N_VAR + ".command_filter}\" process=\"");
			b.append(groupName + "\" action=\"#{" + managedBeanName + "." + filterMethodName + "}\" ");
			b.append("update=\"" + field.getName() + "\" id=\"" + buttonId + "\"/>\n");
			b.append("\t\t</h:panelGrid>\n\n");
		}

		b.append("\t\t<h:outputLabel value=\"\" for=\"" + field.getName() + "\"/>\n");
		b.append("\t\t<p:pickList ");
		b.append("id=\"" + field.getName() + "\" value=\"#{" + managedBeanName + "." + listName + "}\" ");

		if (field.isReadonly())
			b.append("disabled=\"true\" ");

		b.append("converter=\"#{" + converterName + "}\" ");
		b.append("addLabel=\"#{" + EL_I18N_VAR + ".pick_list_add}\" ");
		b.append("addAllLabel=\"#{" + EL_I18N_VAR + ".pick_list_add_all}\" ");
		b.append("removeLabel=\"#{" + EL_I18N_VAR + ".pick_list_remove}\" ");
		b.append("removeAllLabel=\"#{" + EL_I18N_VAR + ".pick_list_remove_all}\" ");
		b.append("var=\"" + varName + "\" itemValue=\"#{" + varName + "}\" itemLabel=\"#{");
		b.append(varName + "." + displayAttr.getModelFieldName() + "}\"/>\n");
		b.append(addToolTipFragment(field.getName()));

		if (searchable)
			b.append("\t\t</h:panelGroup>\n");

		b.append("\n");

		return b.toString();
	}

}
