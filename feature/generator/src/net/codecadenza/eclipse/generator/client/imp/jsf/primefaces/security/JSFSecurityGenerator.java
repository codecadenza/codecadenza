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
package net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.security;

import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.CSS_MANDATORY_FIELD;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.SESSION_OWNER;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.USER_SESSION_BEAN;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import net.codecadenza.eclipse.generator.client.common.security.ISecurityHelper;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.generator.common.CommonGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.Role;

/**
 * <p>
 * Utility class for security-related source code generation
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFSecurityGenerator implements ISecurityHelper {
	private boolean addSecurity;

	/**
	 * Constructor
	 * @param project
	 */
	public JSFSecurityGenerator(Project project) {
		final DTOBean logOnDTO = project.getApplicationLogOnDTO();
		final BoundaryBean logOnBoundary = project.getLogOnBoundary();

		if (logOnDTO != null && logOnBoundary != null)
			this.addSecurity = true;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.security.ISecurityHelper#wrapSecurityCode(java.util.List,
	 * java.lang.String)
	 */
	@Override
	public String wrapSecurityCode(List<Role> roles, String source) {
		final var b = new StringBuilder();
		final boolean hasRole = !roles.isEmpty();
		final boolean addBlock = source.length() - source.replace(";", "").length() > 1;

		if (addSecurity && hasRole) {
			boolean isFirstRole = true;

			for (final Role role : roles) {
				final var roleName = "ROLE_" + role.getName().toUpperCase();

				if (isFirstRole) {
					b.append("if(" + USER_SESSION_BEAN + ".checkAuthorization(false, " + roleName);

					isFirstRole = false;
				}
				else
					b.append(", " + roleName);
			}

			b.append("))\n");

			if (addBlock)
				b.append("{\n");
		}

		if ((addSecurity && hasRole) || !addSecurity)
			b.append(source);

		if (addSecurity && hasRole && addBlock)
			b.append("}\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.security.ISecurityHelper#getSecurityImports()
	 */
	@Override
	public Set<String> getSecurityImports() {
		return Collections.emptySet();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.security.ISecurityHelper#isSecurityAdded()
	 */
	@Override
	public boolean isSecurityAdded() {
		return addSecurity;
	}

	/**
	 * @param roles
	 * @param readOnlyCheck
	 * @return the generated content
	 */
	public String addSecurityCode(List<Role> roles, String readOnlyCheck) {
		final var b = new StringBuilder();
		final boolean hasRole = !roles.isEmpty();

		if (addSecurity) {
			if (!hasRole) {
				b.append(" rendered=\"false\" ");
				return b.toString();
			}

			boolean isFirstRole = true;

			for (final Role role : roles) {
				final String roleName = role.getName().toUpperCase();

				if (isFirstRole) {
					b.append(" rendered=\"#{" + USER_SESSION_BEAN + ".checkAuthorizationString('" + roleName);

					isFirstRole = false;
				}
				else
					b.append(";" + roleName);
			}

			b.append("')");

			if (readOnlyCheck != null && !readOnlyCheck.isEmpty())
				b.append(" and not " + readOnlyCheck);

			b.append("}\" ");
		}
		else if (readOnlyCheck != null && !readOnlyCheck.isEmpty())
			b.append(" rendered=\"#{not " + readOnlyCheck + "}\" ");
		else
			b.append(" ");

		return b.toString();
	}

	/**
	 * @param roles
	 * @return the generated content
	 */
	public String addSecurityCode(List<Role> roles) {
		return addSecurityCode(roles, null);
	}

	/**
	 * @param form
	 * @return the generated content
	 */
	public String addClientCheck(Form form) {
		final Project project = form.getDTO().getNamespace().getProject();
		final DomainObject clientDomainObject = project.getDomainObjectByTag(DomainTagEnumeration.CLIENT);
		final DomainObject domainObject = form.getDomainObject();
		var b = new StringBuilder();
		String clientGetter = null;

		if (!addSecurity)
			return b.toString();

		if (form.getFormType() == FormTypeEnumeration.ADD || form.getFormType() == FormTypeEnumeration.CREATE)
			return b.toString();

		if (!domainObject.isMandated())
			return b.toString();

		b.append("\n");
		b.append("// Check if the logged on user and the selected domain object belong to the same client!\n");
		b.append("if(!" + USER_SESSION_BEAN + ".checkClient(");
		b.append(domainObject.getLowerCaseName());

		if (project.isBoundaryMode()) {
			for (final DTOBeanAttribute dtoAttr : form.getDTO().getAttributes()) {
				final AbstractDomainAssociation assoc = dtoAttr.getAssociation();
				final DTOBean refDTO = dtoAttr.getReferencedDTOBean();

				if (assoc == null)
					continue;

				if (dtoAttr.getDomainAttribute() != null) {
					if (!assoc.getTarget().equals(clientDomainObject) || !dtoAttr.getDomainAttribute().isPk())
						continue;

					clientGetter = "." + dtoAttr.getGetterName();
					break;
				}
				else if (refDTO != null && refDTO.getDomainObject().equals(clientDomainObject)) {
					clientGetter = "." + dtoAttr.getGetterName() + "." + refDTO.getPKAttribute().getGetterName();
					break;
				}
			}
		}
		else
			clientGetter = CommonGenerator.getClientGetterFragment(domainObject, "");

		if (clientGetter == null || clientGetter.isEmpty()) {
			b = new StringBuilder();
			b.append("\n");
			b.append("// Check if the logged on user and the selected domain object belong to the same client!\n");
			b.append("// ATTENTION: The generator was not able to add a client security check! Please consider to add it manually.\n");
			b.append("\n");

			return b.toString();
		}

		b.append(clientGetter);
		b.append("))\n");
		b.append("return;\n\n");

		return b.toString();
	}

	/**
	 * @param roles
	 * @return the generated content
	 */
	public String addSecurityCheck(List<Role> roles) {
		final var b = new StringBuilder();
		final boolean hasRole = !roles.isEmpty();

		if (!addSecurity)
			return b.toString();

		b.append("\n");

		if (hasRole) {
			boolean isFirstRole = true;

			b.append("// Check if user is allowed to open this page!\n");

			for (final Role role : roles) {
				final var roleName = "ROLE_" + role.getName().toUpperCase();

				if (isFirstRole) {
					b.append("if(!" + USER_SESSION_BEAN + ".checkAuthorization(true, " + roleName);

					isFirstRole = false;
				}
				else
					b.append(", " + roleName);
			}

			b.append("))\n");
		}
		else {
			b.append("// ATTENTION: No role has been specified for this page! All requests will be denied!\n");
			b.append("if(!" + USER_SESSION_BEAN + ".checkAuthorization(true, new String[0]))\n");
		}

		b.append("return;\n\n");

		return b.toString();
	}

	/**
	 * Create the content of the login.xhtml file
	 * @param project
	 * @return the generated content
	 */
	public static String createLoginPage(Project project) {
		final var b = new StringBuilder();
		var userNameAttr = "";
		var userPasswordAttr = "";

		for (final DTOBeanAttribute attr : project.getApplicationLogOnDTO().getAttributes()) {
			if (attr.getDomainAttribute() == null)
				continue;

			if (attr.getDomainAttribute().getTag() == AttributeTagEnumeration.USER_NAME)
				userNameAttr = attr.getName();
			else if (attr.getDomainAttribute().getTag() == AttributeTagEnumeration.USER_PASSWORD)
				userPasswordAttr = attr.getName();
		}

		b.append(JSFGeneratorUtil.createXHTMLDocumentRoot());
		b.append(JSFGeneratorUtil.createHeader(true, false, null, null, null));
		b.append("<body>\n\n");
		b.append("\t<p:growl id=\"messages\" showDetail=\"true\" life=\"3000\"/>\n");
		b.append("\t<h:form id=\"form\">\n\n");
		b.append("\t<p:dialog id=\"dialog\" visible=\"true\" header=\"#{i18n.login_title}\" ");
		b.append("widgetVar=\"dlg\" modal=\"true\" closable=\"false\">\n");
		b.append("\t\t<h:panelGrid columns=\"2\" cellpadding=\"3\">\n\n");
		b.append("\t\t\t<h:outputLabel styleClass=\"" + CSS_MANDATORY_FIELD + "\" value=\"#{i18n.login_username} *:\"/>\n");
		b.append("\t\t\t<p:inputText id=\"txtUserName\" value=\"#{" + USER_SESSION_BEAN + "." + SESSION_OWNER);
		b.append("." + userNameAttr + "}\" required=\"true\">\n");
		b.append("\t\t\t\t<f:validateRequired/>\n");
		b.append("\t\t\t</p:inputText>\n\n");
		b.append("\t\t\t<h:outputLabel styleClass=\"" + CSS_MANDATORY_FIELD + "\" value=\"#{i18n.login_password} *:\"/>\n");
		b.append("\t\t\t<p:password id=\"txtPassword\" feedback=\"false\" required=\"true\" value=\"#{");
		b.append(USER_SESSION_BEAN + "." + SESSION_OWNER + "." + userPasswordAttr + "}\">\n");
		b.append("\t\t\t\t<f:validateRequired/>\n");
		b.append("\t\t\t</p:password>\n\n");
		b.append("\t\t\t<h:outputLabel value=\"#{i18n.login_language} *:\" styleClass=\"");
		b.append(CSS_MANDATORY_FIELD + "\" for=\"cboLocale\"/>\n");
		b.append("\t\t\t<p:selectOneMenu id=\"cboLocale\" valueChangeListener=\"#{" + USER_SESSION_BEAN);
		b.append(".onLocaleChanged}\" value=\"#{" + USER_SESSION_BEAN + ".localeCode}\">\n");
		b.append("\t\t\t\t<f:selectItems value=\"#{" + USER_SESSION_BEAN + ".supportedLocales}\"/>\n");
		b.append("\t\t\t</p:selectOneMenu>\n\n");
		b.append("\t\t\t<p:commandButton id=\"cmdLogin\" value=\"#{i18n.login_command}\" ");
		b.append("ajax=\"false\" action=\"#{" + USER_SESSION_BEAN + ".login}\"/>\n\n");
		b.append("\t\t</h:panelGrid>\n");
		b.append("\t</p:dialog>\n");
		b.append("\t</h:form>\n");
		b.append("</body>\n");
		b.append("</html>\n");

		return b.toString();
	}

}
