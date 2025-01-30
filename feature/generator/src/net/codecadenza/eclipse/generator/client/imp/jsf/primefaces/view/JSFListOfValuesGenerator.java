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
package net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.view;

import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.FORM_TITLE;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.SEL_OBJ_ID;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.TRANSLATION_KEYS_CLASS;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.USER_SESSION_BEAN;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.USER_SESSION_BEAN_TYPE;
import static net.codecadenza.eclipse.shared.Constants.EL_I18N_VAR;

import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFI18NGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for list-of-values dialogs of a JSF application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFListOfValuesGenerator extends AbstractJavaSourceGenerator {
	private final Form form;
	private final Project project;
	private final JSFI18NGenerator i18n;
	private final DTOBean dto;
	private final JavaType idAttributeType;
	private final String listName;
	private final String selItemName;
	private final String rowSelectListener;
	private final String managedBeanName;
	private final String fetchMethodName;
	private final String dataTableId;
	private final FormPanel panel;
	private TableColumnField displayColumn;

	/**
	 * Constructor
	 * @param form
	 */
	public JSFListOfValuesGenerator(Form form) {
		super(form.getSourceFile());

		this.form = form;
		this.dto = form.getDTO();
		this.project = form.getDomainObject().getNamespace().getProject();
		this.i18n = new JSFI18NGenerator(project);
		this.idAttributeType = dto.getPKAttribute().getDomainAttribute().getJavaType();
		this.listName = dto.getDomainObject().getNamePlural().substring(0, 1).toLowerCase()
				+ dto.getDomainObject().getNamePlural().substring(1) + "List";
		this.selItemName = SEL_OBJ_ID;
		this.rowSelectListener = "onRowSelect";
		this.managedBeanName = JSFGeneratorUtil.createManagedBeanName(form.getName());
		this.fetchMethodName = "fetch" + dto.getDomainObject().getNamePlural();
		this.dataTableId = "dataTable";
		this.panel = form.getViewFormPanel();

		for (final TableColumnField col : panel.getFormTable().getFields())
			if (col.getDTOAttribute().isLovReturn()) {
				this.displayColumn = col;
				break;
			}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + TRANSLATION_KEYS_CLASS);
		importStatic(project.getClientNamespace().toString() + "." + USER_SESSION_BEAN_TYPE);
		importPackage(project.getClientNamespace().toString());
		importPackage(dto.getNamespace().toString());
		importPackage("java.util");
		importPackage("org.primefaces.event");

		// Add imports for all enumerations
		for (final TableColumnField col : panel.getFormTable().getFields()) {
			if (!col.isSearchable() || col.getFieldType() != TableColumnFieldTypeEnumeration.ENUM)
				continue;

			final var javaEnum = (JavaEnum) col.getDTOAttribute().getDomainAttribute().getJavaType();

			importPackage(javaEnum.getNamespace().toString());
		}

		importClass("jakarta.faces.application.FacesMessage");
		importPackage("net.codecadenza.runtime.webclient.primefaces.util");
		importPackage("java.io");
		importPackage("jakarta.faces.view");
		importPackage("jakarta.inject");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@Named(\"" + JSFGeneratorUtil.createManagedBeanName(form.getName()) + "\")\n");
		b.append("@ViewScoped\n");
		b.append("public class ");
		b.append(form.getName() + " implements Serializable");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		final String defaultValue = dto.getPKAttribute().getDomainAttribute().getEmptyItemDefaultValue();

		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();
		addPrivateField("List<" + dto.getName() + ">", listName).withDefaultValue("new ArrayList<>()").create();
		addPrivateField(idAttributeType.getName(), selItemName).withDefaultValue(defaultValue).create();

		new ServiceDeclarationGenerator(this, form.getBoundaryMethod().getBoundaryBean()).addField();

		addPrivateField(USER_SESSION_BEAN_TYPE, USER_SESSION_BEAN).inject().create();
		addPrivateField(JavaType.STRING, FORM_TITLE).withDefaultValue("\"\"").create();
		addPrivateField(JavaType.STRING, "idElementId").create();
		addPrivateField(JavaType.STRING, "displayElementId").create();
		addPrivateField(JavaType.STRING, "selectedDisplayValue").create();
		addPrivateField(JavaType.STRING, "filter").create();
		addPrivateField("ResourceBundle", "bundle").withTransientModifier().create();

		if (idAttributeType.isString())
			addPrivateConstant(JavaType.INT, "MIN_FILTER_LENGTH", "2").create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		StringBuilder b;

		addGetter("Collection<" + dto.getName() + ">", listName, "the list of elements");
		addGetterAndSetter(idAttributeType.getName(), selItemName, "the selected item id");
		addGetterAndSetter(JavaType.STRING, FORM_TITLE, "the form title");

		// Add translation methods for enumeration fields
		for (final TableColumnField col : panel.getFormTable().getFields()) {
			if (!col.isSearchable() || col.getFieldType() != TableColumnFieldTypeEnumeration.ENUM)
				continue;

			final var transMethodName = "translate" + col.getDTOAttribute().getUpperCaseName();
			final var javaEnum = (JavaEnum) col.getDTOAttribute().getDomainAttribute().getJavaType();

			// Generate translations for all literals
			javaEnum.getEnumerationValues().forEach(i18n::getI18N);

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * Perform translation of given enumeration literal\n");
			b.append(" * @param item\n");
			b.append(" * @return the translation based on the user's locale\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public String " + transMethodName + "(" + javaEnum.getName() + " item)\n");
			b.append("{\n");
			b.append("return bundle.getString(\"" + javaEnum.getName().toLowerCase() + "_\" + item.name().toLowerCase());\n");
			b.append("}\n\n");

			addMethod("String " + transMethodName + "(" + javaEnum.getName() + " item)", b.toString());
		}

		// Create a listener method for row selection change events!
		final String pkGetter = dto.getPKAttribute().getGetterName();

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Event that will be fired if user selects an element\n");
		b.append(" * @param event\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public void " + rowSelectListener + "(SelectEvent<" + dto.getName() + "> event)\n");
		b.append("{\n");
		b.append(selItemName + " = event.getObject()." + pkGetter + ";\n");

		if (displayColumn != null) {
			b.append("selectedDisplayValue = event.getObject().");
			b.append(displayColumn.getDTOAttribute().getGetterName() + ";\n");
		}

		b.append("}\n\n");

		addMethod("void " + rowSelectListener + "(SelectEvent<" + dto.getName() + "> event)", b.toString());

		final var methodSignature = "void " + fetchMethodName + "()";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Filter items to be displayed in the list-of-values dialog\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append(listName + " = Collections.emptyList();\n\n");

		if (idAttributeType.isString()) {
			// For filters of type String we expect a minimum number of input characters before starting a search operation!
			b.append("// We expect a minimum number of input characters before starting a search operation!\n");
			b.append("if(filter == null || filter.length() < MIN_FILTER_LENGTH)\n");
		}
		else
			b.append("if(filter == null || filter.isEmpty())\n");

		b.append("return;\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append(listName + " = ");

		new ServiceInvocationGenerator(form.getBoundaryMethod(), dto, b).addInvocation("filter");

		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Error while fetching data!", "e");

		b.append("\n");
		b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_FETCH_FAIL, e);\n");
		b.append("}\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		addGetterAndSetter(JavaType.STRING, "idElementId", "the id of the element that holds the referenced primary key value");
		addGetterAndSetter(JavaType.STRING, "displayElementId", "the id of the element that holds the display value");
		addGetterAndSetter(JavaType.STRING, "selectedDisplayValue", "the display value");
		addGetterAndSetter(JavaType.STRING, "filter", "the filter string");

		final var declaration = "void initView()";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Initialize view\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + declaration + "\n");
		b.append("{\n");

		addDebugLog(b, "Initialize dialog");

		b.append("\n");
		b.append("bundle = ResourceBundle.getBundle(DEFAULT_BUNDLE_NAME, " + USER_SESSION_BEAN + ".getLocale());\n");
		b.append(FORM_TITLE + " = " + i18n.getI18NBundleFragment(form) + ";\n\n");
		b.append("if(displayElementId == null)\n");
		b.append("displayElementId = \"\";\n\n");

		addDebugLog(b, "Dialog initialization finished");

		b.append("}\n\n");

		addMethod(declaration, b.toString());

		i18n.save();
	}

	/**
	 * Create the XHTML form content
	 * @return the generated content
	 */
	public String createXHTMLForm() {
		final var b = new StringBuilder();
		String selectMethodName = "setSelectedStringItem";
		String resetMethodName = "resetSelectedStringItem";

		if (idAttributeType.isInteger()) {
			selectMethodName = "setSelectedIntegerItem";
			resetMethodName = "resetSelectedIntegerItem";
		}
		else if (idAttributeType.isLong()) {
			selectMethodName = "setSelectedLongItem";
			resetMethodName = "resetSelectedLongItem";
		}

		b.append(JSFGeneratorUtil.createXHTMLDocumentRoot());
		b.append(JSFGeneratorUtil.createHeader(true, true, i18n.getI18N(form), null, null));
		b.append("<f:metadata>\n");
		b.append("\t<f:viewAction action=\"#{" + managedBeanName + ".initView()}\"/>\n");
		b.append("\t<f:viewParam name=\"idElementId\" value=\"#{" + managedBeanName + ".idElementId}\"/>\n");
		b.append("\t<f:viewParam name=\"displayElementId\" value=\"#{" + managedBeanName + ".displayElementId}\"/>\n");
		b.append("</f:metadata>\n\n");
		b.append("<body class=\"lov\">\n");
		b.append("<h:form id=\"form\">\n\n");
		b.append("\t<h:panelGrid columns=\"3\">\n");
		b.append("\t\t<div class=\"pi pi-search\" style=\"font-size: 2em\"/>\n");

		// Entity &#160; is equal to &nbsp;!
		b.append("\t\t<h:outputText id=\"lblFormTitle\" value=\"&#160;#{" + managedBeanName);
		b.append("." + FORM_TITLE + "}\" styleClass=\"label-form-title\"/>\n");

		b.append("\t\t<p:ajaxStatus style=\"width:16px;height:16px;\">\n");
		b.append("\t\t\t<f:facet name=\"start\">\n");
		b.append("\t\t\t\t<h:graphicImage value=\"/images/ajaxloading.gif\"/>\n");
		b.append("\t\t\t</f:facet>\n\n");
		b.append("\t\t\t<f:facet name=\"complete\">\n");
		b.append("\t\t\t\t<h:outputText value=\"\"/>\n");
		b.append("\t\t\t</f:facet>\n");
		b.append("\t\t</p:ajaxStatus>\n");
		b.append("\t</h:panelGrid>\n");
		b.append("\t<p/>\n\n");
		b.append("\t<p:dialog id=\"msgbox\" header=\"#{" + EL_I18N_VAR);
		b.append(".feedback_dialog_header}\" modal=\"true\" visible=\"#{(not empty facesContext.messageList)}\">\n");
		b.append("\t\t<p:messages showDetail=\"true\" showSummary=\"true\"/>\n");
		b.append("\t</p:dialog>\n\n");
		b.append("\t<p:focus for=\"txtInput\"/>\n");
		b.append("\t<h:panelGrid columns=\"2\">\n");
		b.append("\t\t<h:outputLabel styleClass=\"label-field-mandatory\" value=\"#{");
		b.append(EL_I18N_VAR + ".label_lookup}&#160;\" for=\"txtInput\"/>\n");
		b.append("\t\t<p:inputText autocomplete=\"off\" id=\"txtInput\" value=\"#{");
		b.append(managedBeanName + ".filter}\" size=\"40\">\n");
		b.append("\t\t\t<p:ajax event=\"keyup\" update=\":form:" + dataTableId + ",:form:msgbox,:form:numberOfRecordsets\" ");
		b.append("listener=\"#{" + managedBeanName + "." + fetchMethodName + "}\"/>\n");
		b.append("\t\t</p:inputText>\n");
		b.append("\t</h:panelGrid>\n");
		b.append("\t<p/>\n\n");
		b.append("\t<p:scrollPanel styleClass=\"lov-scrollpanel\" mode=\"native\">\n");
		b.append("\t<p:dataTable id=\"" + dataTableId + "\" var=\"lovItem\" rows=\"1000\"\n");
		b.append("\t\tloadingMessage=\"#{" + EL_I18N_VAR + ".action_fetch_data}\"\n");
		b.append("\t\tvalue=\"#{" + managedBeanName + "." + listName + "}\" scrollable=\"false\"\n");
		b.append("\t\tpaginator=\"false\" resizableColumns=\"true\" selectionMode=\"single\"\n");
		b.append("\t\trowKey=\"#{lovItem." + dto.getPKAttribute().getName() + "}\">\n");
		b.append("\t\t<p:ajax update=\":form:" + selItemName + ",:form:selectedDisplayValue\" ");
		b.append("event=\"rowSelect\" listener=\"#{" + managedBeanName + "." + rowSelectListener + "}\"/>\n");
		b.append("\t\t<f:facet name=\"header\">");
		b.append("\n");
		b.append("\t\t</f:facet>\n");

		// Sort all table columns
		final EList<TableColumnField> cols = panel.getFormTable().getFields();

		ECollections.sort(cols, (col1, col2) -> col1.getColIndex() - col2.getColIndex());

		// Add all visible table columns
		for (final TableColumnField col : cols) {
			if (!col.isVisible())
				continue;

			b.append("\n\t\t<p:column sortBy=\"#{lovItem." + col.getDTOAttribute().getName());
			b.append("}\" width=\"" + col.getWidth() + "\">\n");
			b.append("\t\t\t<f:facet name=\"header\">\n");
			b.append("\t\t\t\t<h:outputText value=\"" + i18n.getI18N(col) + "\"/>\n");
			b.append("\t\t\t</f:facet>\n");
			b.append("\t\t\t<h:outputText value=\"#{");

			if (col.getDTOAttribute().getDomainAttribute().getJavaType().isEnum()) {
				// The enumeration literal value must be translated by a respective backing bean method!
				final var transMethodName = "translate" + col.getDTOAttribute().getUpperCaseName();

				b.append(managedBeanName + "." + transMethodName + "(");
				b.append("lovItem." + col.getDTOAttribute().getName() + ")}\">\n");
			}
			else {
				b.append("lovItem." + col.getDTOAttribute().getName());

				if (col.getFieldType() == TableColumnFieldTypeEnumeration.GREGORIAN_CALENDAR)
					b.append(".time");

				b.append("}\">\n");

				if (col.getFieldType() == TableColumnFieldTypeEnumeration.DATE
						|| col.getFieldType() == TableColumnFieldTypeEnumeration.GREGORIAN_CALENDAR) {
					if (col.hasDateFormat())
						b.append("\t\t\t\t<f:convertDateTime pattern=\"#{" + USER_SESSION_BEAN + ".dateFormat}\" ");
					else
						b.append("\t\t\t\t<f:convertDateTime pattern=\"#{" + USER_SESSION_BEAN + ".dateTimeFormat}\" ");

					b.append("timeZone=\"#{" + USER_SESSION_BEAN + ".timeZone}\"/>\n");
				}
				else if (col.getFieldType() == TableColumnFieldTypeEnumeration.DOUBLE
						|| col.getFieldType() == TableColumnFieldTypeEnumeration.FLOAT
						|| col.getFieldType() == TableColumnFieldTypeEnumeration.BIG_DECIMAL)
					b.append("\t\t\t\t<f:convertNumber pattern=\"#{" + USER_SESSION_BEAN + ".numberFormat}\"/>\n");
				else if (col.getFieldType() == TableColumnFieldTypeEnumeration.LOCAL_DATE) {
					b.append("\t\t\t\t<f:converter converterId=\"");
					b.append("net.codecadenza.runtime.webclient.primefaces.converter.LocalDateConverter\"/>\n");
					b.append("\t\t\t\t<f:attribute name=\"pattern\" value=\"#{" + USER_SESSION_BEAN + ".dateFormat}\"/>\n");
				}
				else if (col.getFieldType() == TableColumnFieldTypeEnumeration.LOCAL_DATE_TIME) {
					b.append("\t\t\t\t<f:converter converterId=\"");
					b.append("net.codecadenza.runtime.webclient.primefaces.converter.LocalDateTimeConverter\"/>\n");
					b.append("\t\t\t\t<f:attribute name=\"pattern\" value=\"#{" + USER_SESSION_BEAN + ".dateTimeFormat}\"/>\n");
				}

				if (col.getFieldType() == TableColumnFieldTypeEnumeration.LOCAL_DATE
						|| col.getFieldType() == TableColumnFieldTypeEnumeration.LOCAL_DATE_TIME)
					b.append("\t\t\t\t<f:attribute name=\"timeZone\" value=\"#{" + USER_SESSION_BEAN + ".timeZone}\"/>\n");
			}

			b.append("\t\t\t</h:outputText>\n");
			b.append("\t\t</p:column>\n");
		}

		b.append("\t</p:dataTable>\n");
		b.append("\t</p:scrollPanel>\n");
		b.append("\t<p/>\n\n");
		b.append("\t<h:outputText id=\"numberOfRecordsets\" styleClass=\"label-field-mandatory\" value=\"#{" + EL_I18N_VAR);
		b.append(".result_total_number_records} #{" + managedBeanName + "." + listName + ".size()}\"/>\n");
		b.append("\t<p/>\n\n");
		b.append("\t<h:panelGrid columns=\"3\">\n");
		b.append("\t\t<p:commandButton value=\"#{" + EL_I18N_VAR + ".command_cancel}\" onclick=\"window.close()\"/>\n");
		b.append("\t\t<p:commandButton id=\"cmdSelect\" value=\"#{" + EL_I18N_VAR);
		b.append(".command_select}\" icon=\"pi pi-check\" onclick=\"" + selectMethodName + "()\"/>\n");
		b.append("\t\t<p:commandButton id=\"cmdReset\" value=\"#{" + EL_I18N_VAR);
		b.append(".command_reset}\" icon=\"pi pi-minus\" onclick=\"" + resetMethodName + "()\"/>\n");
		b.append("\t</h:panelGrid>\n");
		b.append("\n");
		b.append("\t<h:inputHidden id=\"" + selItemName + "\" value=\"#{" + managedBeanName + "." + selItemName + "}\"/>\n");
		b.append("\t<h:inputHidden id=\"selectedDisplayValue\" value=\"#{" + managedBeanName + ".selectedDisplayValue}\"/>\n");
		b.append("\t<h:inputHidden id=\"idElementId\" value=\"#{" + managedBeanName + ".idElementId}\"/>\n");
		b.append("\t<h:inputHidden id=\"displayElementId\" value=\"#{" + managedBeanName + ".displayElementId}\"/>\n\n");
		b.append("\t<p:defaultCommand target=\":form:cmdSelect\"/>\n\n");
		b.append("</h:form>\n");
		b.append("</body>\n");
		b.append("</html>\n");

		i18n.save();

		return b.toString();
	}

}
