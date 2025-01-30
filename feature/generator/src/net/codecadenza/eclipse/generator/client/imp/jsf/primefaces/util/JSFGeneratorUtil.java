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
package net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util;

import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.CSS_MANDATORY_FIELD;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.CSS_OPTIONAL_FIELD;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.JSF_SUFFIX;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.USER_SESSION_BEAN;
import static net.codecadenza.eclipse.shared.Constants.UI_DIALOG_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.UI_TREE_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.UI_VIEW_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.UTF_8;

import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Utility class for JSF generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFGeneratorUtil {
	/**
	 * Prevent instantiation
	 */
	private JSFGeneratorUtil() {

	}

	/**
	 * @param form
	 * @return the relative URL
	 */
	public static String getFormRelativeURL(Form form) {
		if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW || form.getFormType() == FormTypeEnumeration.SIMPLE_VIEW)
			return UI_VIEW_FOLDER + "/" + form.getName() + JSF_SUFFIX;
		else if (form.getFormType() == FormTypeEnumeration.TREE_VIEW)
			return UI_TREE_FOLDER + "/" + form.getName() + JSF_SUFFIX;
		else
			return UI_DIALOG_FOLDER + "/" + form.getName() + JSF_SUFFIX;
	}

	/**
	 * @param form
	 * @return the URL fragment of the given form
	 */
	public static final String getFormURLFragment(Form form) {
		if (form.getFormType() == FormTypeEnumeration.SEARCHABLE_VIEW || form.getFormType() == FormTypeEnumeration.SIMPLE_VIEW)
			return "#{request.contextPath}" + UI_VIEW_FOLDER + "/" + form.getName() + JSF_SUFFIX;
		else if (form.getFormType() == FormTypeEnumeration.TREE_VIEW)
			return "#{request.contextPath}" + UI_TREE_FOLDER + "/" + form.getName() + JSF_SUFFIX;
		else
			return "#{request.contextPath}" + UI_DIALOG_FOLDER + "/" + form.getName() + JSF_SUFFIX;
	}

	/**
	 * @param form
	 * @return the URL fragment of the given form including the ID parameter
	 */
	public static final String getFormURLFragmentWithId(Form form) {
		return getFormURLFragment(form) + "?selectedObjectId=";
	}

	/**
	 * Create the XHTML document root
	 * @return the generated content
	 */
	public static final String createXHTMLDocumentRoot() {
		final var b = new StringBuilder();
		b.append("<?xml version=\"1.0\" encoding=\"" + UTF_8 + "\"?>\n");
		b.append("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" ");
		b.append("\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n");
		b.append("<html xmlns=\"http://www.w3.org/1999/xhtml\"\n");
		b.append("xmlns:h=\"jakarta.faces.html\"\n");
		b.append("xmlns:fn=\"jakarta.tags.functions\"\n");
		b.append("xmlns:f=\"jakarta.faces.core\"\n");
		b.append("xmlns:p=\"http://primefaces.org/ui\">\n\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	public static final String createCompositeHeader() {
		final var b = new StringBuilder();
		b.append("<ui:composition xmlns=\"http://www.w3.org/1999/xhtml\"\n");
		b.append("\txmlns:ui=\"jakarta.faces.facelets\"\n");
		b.append("\txmlns:h=\"jakarta.faces.html\"\n");
		b.append("\txmlns:f=\"jakarta.faces.core\"\n");
		b.append("\txmlns:p=\"http://primefaces.org/ui\"\n");
		b.append("\ttemplate=\"/templates/ui.xhtml\">\n\n");

		return b.toString();
	}

	/**
	 * @param field
	 * @param i18n
	 * @param altName
	 * @param render
	 * @return the generated content
	 */
	public static final String createFieldLabel(FormField field, JSFI18NGenerator i18n, String altName, String render) {
		final var b = new StringBuilder();
		b.append("\t\t<h:outputLabel value=\"" + i18n.getI18N(field));

		if (field.isMandatory() && !field.isReadonly())
			b.append(" *");

		b.append(":\"");

		if (render != null && !render.isEmpty())
			b.append(render);
		else
			b.append(" ");

		if (altName == null || altName.isEmpty())
			b.append("for=\"" + field.getName() + "\" styleClass=\"");
		else
			b.append("for=\"" + altName + "\" styleClass=\"");

		if (field.isMandatory() && !field.isReadonly())
			b.append(CSS_MANDATORY_FIELD);
		else
			b.append(CSS_OPTIONAL_FIELD);

		b.append("\"/>\n");

		return b.toString();
	}

	/**
	 * @param field
	 * @return the form that is used as target for a label link
	 */
	public static Form getFormForLink(FormField field) {
		final Project project = field.getDTOAttribute().getDTOBean().getNamespace().getProject();

		if (field.isAddFormLinkToLabel()) {
			for (final Form f : project.getAllFormsOfProject()) {
				if (!f.getDomainObject().equals(field.getDTOAttribute().getReferencedDTOBean().getDomainObject()))
					continue;

				if (f.getFormType() == FormTypeEnumeration.READONLY)
					return f;
			}
		}

		return null;
	}

	/**
	 * Determine the boundary method to be used for auto-complete functionality for the given domain attribute
	 * @param attr
	 * @return the corresponding boundary method or null if no appropriate method exists
	 */
	public static BoundaryMethod getAutoCompleteMethod(DomainAttribute attr) {
		final DomainObject domainObject = attr.getDomainObject();
		final Project project = domainObject.getNamespace().getProject();
		final DomainAttribute displayAttr = domainObject.getDisplayAttribute();
		final BoundaryBean boundary = project.getBoundaryByDomainObject(attr.getDomainObject());

		if (!attr.isWildcardFilteringSupported())
			return null;

		if (displayAttr != null) {
			// The attribute must represent the display attribute of this domain object!
			if (!attr.equals(displayAttr))
				return null;
		}
		else if (!attr.equals(domainObject.getPKAttribute())) {
			// The attribute must represent the primary key attribute of this domain object!
			return null;
		}

		if (boundary != null)
			for (final BoundaryMethod m : boundary.getBoundaryMethods()) {
				// We assume that there is just one method of type 'SEARCH_BY_FILTER'!
				if (m.getMethodType() == BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER)
					return m;
			}

		return null;
	}

	/**
	 * @param field
	 * @param i18n
	 * @return the generated content
	 */
	public static final String createFieldLabel(FormField field, JSFI18NGenerator i18n) {
		return createFieldLabel(field, i18n, null, null);
	}

	/**
	 * Create the header fragment
	 * @param addDefaultCSS
	 * @param includeJSUtility
	 * @param title
	 * @param bodyStyle
	 * @param jScript
	 * @return the generated comment
	 */
	public static final String createHeader(boolean addDefaultCSS, boolean includeJSUtility, String title, String bodyStyle,
			String jScript) {
		final var b = new StringBuilder();
		b.append("<f:view locale=\"#{" + USER_SESSION_BEAN + ".locale}\"/>\n");
		b.append("<h:head>\n");
		b.append("<meta content=\"text/html; charset=" + UTF_8 + "\" http-equiv=\"Content-Type\"/>\n");
		b.append("<meta http-equiv=\"refresh\" content=\"#{session.maxInactiveInterval};");
		b.append("url=#{request.scheme}://#{request.serverName}:#{request.serverPort}#{request.contextPath}\"/>\n");

		if (addDefaultCSS)
			b.append("<link type=\"text/css\" rel=\"stylesheet\" href=\"#{request.contextPath}/css/default.css\"/>\n");

		if (includeJSUtility)
			b.append("<script type=\"text/javascript\" src=\"#{request.contextPath}/script/util.js\"/>\n");

		if (title != null && !title.isEmpty())
			b.append("<title>" + title + "</title>\n");

		if (bodyStyle != null && !bodyStyle.isEmpty()) {
			b.append("<style type=\"text/css\">\n");
			b.append("\tbody\n\t{\n");
			b.append(bodyStyle);
			b.append("\t}\n");
			b.append("</style>\n");
		}

		if (jScript != null && !jScript.isEmpty()) {
			b.append("<script type=\"text/javascript\">\n");
			b.append(jScript);
			b.append("</script>\n");
		}

		b.append("</h:head>\n\n");

		return b.toString();
	}

	/**
	 * @param beanName
	 * @return the standard name (conform to CDI 1.2) of a managed bean
	 */
	public static final String createManagedBeanName(String beanName) {
		return beanName.substring(0, 1).toLowerCase() + beanName.substring(1);
	}

}
