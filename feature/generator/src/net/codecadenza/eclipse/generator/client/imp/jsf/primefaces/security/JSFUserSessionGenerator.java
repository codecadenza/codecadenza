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

import static net.codecadenza.eclipse.generator.basic.client.imp.JSFClientProjectFilesGenerator.USER_SESSION_COMMENT;
import static net.codecadenza.eclipse.generator.basic.client.imp.JSFClientProjectFilesGenerator.USER_SESSION_NAME;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.SESSION_OWNER;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.TRANSLATION_KEYS_CLASS;
import static net.codecadenza.eclipse.shared.Constants.APP_LOGON_DTO_NAME;
import static net.codecadenza.eclipse.shared.Constants.BUNDLE_NAME;
import static net.codecadenza.eclipse.shared.Constants.UI_VIEW_FOLDER;

import java.util.ArrayList;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFI18NGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.Role;

/**
 * <p>
 * Generator for the class that represents a user sessions in a JSF application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFUserSessionGenerator extends AbstractJavaSourceGenerator {
	private final Project project;
	private final DTOBean logOnDTO;
	private final JSFI18NGenerator i18n;

	/**
	 * Constructor
	 * @param project
	 */
	public JSFUserSessionGenerator(Project project) {
		this.project = project;
		this.logOnDTO = project.getApplicationLogOnDTO();
		this.i18n = new JSFI18NGenerator(project);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final var javaFile = new JavaFile(project, BuildArtifactType.GUI, USER_SESSION_NAME, project.getClientNamespace().toString());
		javaFile.setComment(USER_SESSION_COMMENT);

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + TRANSLATION_KEYS_CLASS);
		importPackage(logOnDTO.getNamespace().toString());
		importPackage("java.util");
		importClass("jakarta.annotation.PostConstruct");
		importPackage("jakarta.faces.application");
		importPackage("jakarta.faces.context");
		importPackage("jakarta.faces.event");
		importPackage("jakarta.inject");
		importPackage("jakarta.servlet.http");
		importPackage("java.io");
		importPackage("java.net");
		importPackage("java.nio.charset");
		importPackage("net.codecadenza.runtime.crypto");
		importPackage("net.codecadenza.runtime.webclient.primefaces.util");

		if (project.isSpringBootApplication()) {
			importClass("org.springframework.web.context.annotation.SessionScope");
			importClass("org.springframework.security.web.context.HttpSessionSecurityContextRepository");
			importPackage("org.springframework.security.authentication");
			importPackage("org.springframework.security.core");
			importPackage("org.springframework.security.core.context");
		}
		else {
			importPackage("jakarta.enterprise.context");
			importPackage("jakarta.security.enterprise");
			importPackage("jakarta.security.enterprise.credential");
			importPackage("jakarta.security.enterprise.authentication.mechanism.http");
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@Named(\"" + JSFGeneratorUtil.createManagedBeanName(USER_SESSION_NAME) + "\")\n");

		if (project.isJakartaEEApplication())
			b.append("@SessionScoped\n");
		else
			b.append("@SessionScope\n");

		b.append("public class " + USER_SESSION_NAME + " implements Serializable");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		final var bundleInit = "ResourceBundle.getBundle(DEFAULT_BUNDLE_NAME, locale)";

		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();
		addPublicConstant(JavaType.STRING, "DEFAULT_BUNDLE_NAME", "\"" + BUNDLE_NAME + "\"").create();
		addPublicConstant(JavaType.STRING, "USER_NAME_COOKIE", "\"userName\"").create();
		addPublicConstant(JavaType.STRING, "DATE_FORMAT_COOKIE", "\"dateFormat\"").create();
		addPublicConstant(JavaType.STRING, "DATE_TIME_FORMAT_COOKIE", "\"dateTimeFormat\"").create();
		addPublicConstant(JavaType.STRING, "NUMBER_FORMAT_COOKIE", "\"numberFormat\"").create();
		addPublicConstant(JavaType.STRING, "TIMEZONE_COOKIE", "\"timeZone\"").create();
		addPublicConstant(JavaType.STRING, "START_PAGE", "\"" + UI_VIEW_FOLDER + "/index.jsf?faces-redirect=true\"").create();

		project.getRoles().forEach(role -> {
			final var roleName = "ROLE_" + role.getName();

			addPublicConstant(JavaType.STRING, roleName, "\"" + role.getName() + "\"").create();
		});

		addPrivateField("Map<String, Locale>", "supportedLocales").asConstant(null).create();

		new ServiceDeclarationGenerator(this, project.getLogOnBoundary()).addField();

		final var staticInitBlock = new StringBuilder();
		staticInitBlock.append("\n");
		staticInitBlock.append("static\n");
		staticInitBlock.append("{\n");
		staticInitBlock.append("supportedLocales = new LinkedHashMap<>();\n");
		staticInitBlock.append("supportedLocales.put(\"EN\", Locale.ENGLISH);\n");
		staticInitBlock.append("}\n\n");

		addStaticInitializationBlock(staticInitBlock.toString());

		addPrivateField(APP_LOGON_DTO_NAME, SESSION_OWNER).create();
		addPrivateField(JavaType.STRING, "dateStyle").withDefaultValue("\"medium\"").create();
		addPrivateField(JavaType.STRING, "dateFormat").withDefaultValue("\"dd.MM.yyyy\"").create();
		addPrivateField(JavaType.STRING, "dateTimeFormat").withDefaultValue("\"dd.MM.yyyy HH:mm:ss\"").create();
		addPrivateField(JavaType.STRING, "numberFormat").withDefaultValue("\"###,###,##0.0000\"").create();
		addPrivateField(JavaType.STRING, "localeCode").withDefaultValue("\"EN\"").create();
		addPrivateField(JavaType.STRING, "oldPassword").withDefaultValue("\"\"").create();
		addPrivateField(JavaType.STRING, "newPassword").withDefaultValue("\"\"").create();
		addPrivateField(JavaType.STRING, "confirmedPassword").withDefaultValue("\"\"").create();
		addPrivateField(JavaType.STRING, "timeZone").withDefaultValue("TimeZone.getDefault().getID()").create();
		addPrivateField("Locale", "locale").withDefaultValue("Locale.ENGLISH").create();
		addPrivateField("LinkedList<String>", "history").withDefaultValue("new LinkedList<>()").withFinalModifier().create();
		addPrivateField("ResourceBundle", "bundle").withTransientModifier().withDefaultValue(bundleInit).withFinalModifier().create();

		if (project.isSpringBootApplication())
			addPrivateField("AuthenticationManager", "authenticationManager").withTransientModifier().inject().create();
		else
			addPrivateField("SecurityContext", "securityContext").withTransientModifier().inject().create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final DomainObject clientDomainObject = project.getDomainObjectByTag(DomainTagEnumeration.CLIENT);
		final BoundaryBean logOnBoundary = project.getLogOnBoundary();
		final BoundaryMethod logOnMethod = logOnBoundary.getBoundaryMethodByReturnType(logOnDTO,
				BoundaryMethodTypeEnumeration.LOG_ON);
		final BoundaryMethod changePasswordMethod = logOnBoundary
				.getBoundaryMethodByReturnType(project.getJavaTypeByName(JavaType.VOID), BoundaryMethodTypeEnumeration.CHANGE_PASSWORD);

		addGetterAndSetter(JavaType.STRING, "oldPassword", "the current password");
		addGetterAndSetter(JavaType.STRING, "newPassword", "the new password");
		addGetterAndSetter(JavaType.STRING, "confirmedPassword", "the re-entered password");
		addGetterAndSetter(JavaType.STRING, "timeZone", "the time zone");
		addGetterAndSetter(APP_LOGON_DTO_NAME, SESSION_OWNER, "the user object");
		addGetterAndSetter(JavaType.STRING, "dateStyle", "the date style");
		addGetterAndSetter(JavaType.STRING, "dateFormat", "the date format");
		addGetterAndSetter(JavaType.STRING, "dateTimeFormat", "the date time format");
		addGetterAndSetter(JavaType.STRING, "numberFormat", "the number format");
		addGetterAndSetter(JavaType.STRING, "localeCode", "the locale code");
		addGetterAndSetter("Locale", "locale", "the locale");

		var methodSignature = "Map<String, Locale> getSupportedLocales()";

		var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * @return all supported locales\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("return supportedLocales;\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "String[] getTimeZones()";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * @return all available time zones\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("return TimeZone.getAvailableIDs();\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "void init()";
		var userNameSetter = "";

		for (final DTOBeanAttribute attr : logOnDTO.getAttributes()) {
			if (attr.getDomainAttribute() == null)
				continue;

			if (attr.getDomainAttribute().getTag() == AttributeTagEnumeration.USER_NAME)
				userNameSetter = attr.getSetterName();
		}

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Initialize session and try to read user settings from cookies\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("@PostConstruct\n");
		b.append("public void init()\n");
		b.append("{\n");

		addDebugLog(b, "Initialize user session");

		b.append("\n");
		b.append("final var req = (HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRequest();\n");
		b.append("final Cookie[] cookies = req.getCookies();\n");
		b.append(SESSION_OWNER + " = new " + logOnDTO.getName() + "();\n\n");
		b.append("if(cookies == null)\n");
		b.append("return;\n\n");
		b.append("for(final Cookie cookie : cookies)\n");
		b.append("{\n");
		b.append("String cookieValue = null;\n\n");
		b.append("if(cookie.getValue() == null || cookie.getValue().isEmpty())\n");
		b.append("continue;\n\n");
		b.append("cookieValue = URLDecoder.decode(cookie.getValue(), StandardCharsets.UTF_8);\n\n");
		b.append("if(cookie.getName().equals(USER_NAME_COOKIE))\n");
		b.append(SESSION_OWNER + "." + userNameSetter + "(cookieValue);\n");
		b.append("else if(cookie.getName().equals(DATE_FORMAT_COOKIE))\n");
		b.append("dateFormat = cookieValue;\n");
		b.append("else if(cookie.getName().equals(DATE_TIME_FORMAT_COOKIE))\n");
		b.append("dateTimeFormat = cookieValue;\n");
		b.append("else if(cookie.getName().equals(NUMBER_FORMAT_COOKIE))\n");
		b.append("numberFormat = cookieValue;\n");
		b.append("else if(cookie.getName().equals(TIMEZONE_COOKIE))\n");
		b.append("timeZone = cookieValue;\n");
		b.append("}\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "void onLocaleChanged(ValueChangeEvent e)";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Change event if user selects different locale\n");
		b.append(" * @param e\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("final String newLocaleValue = e.getNewValue().toString().toUpperCase();\n\n");
		b.append("if(supportedLocales.containsKey(newLocaleValue))\n");
		b.append("{\n");
		b.append("locale = supportedLocales.get(newLocaleValue);\n");
		b.append("localeCode = newLocaleValue;\n\n");
		b.append("FacesContext.getCurrentInstance().getViewRoot().setLocale(locale);\n");
		b.append("}\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "String logout()";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * @return the navigation target\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		if (project.isJakartaEEApplication()) {
			b.append("final var req = (HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRequest();\n");
			b.append("req.getSession().invalidate();\n\n");
		}
		else
			b.append("SecurityContextHolder.getContext().setAuthentication(null);\n\n");

		b.append("return START_PAGE;\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "boolean checkAuthorizationString(String roles)";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * @param roles contains all roles delimited by semicolon\n");
		b.append(" * @return true if user is supplied with one of given roles\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("if(roles == null || roles.isEmpty())\n");
		b.append("return false;\n\n");
		b.append("final String[] roleList = roles.split(\";\");\n\n");
		b.append("return checkAuthorization(false, roleList);\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "boolean checkAuthorization(boolean sendError, String... roleList)";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * @param sendError\n");
		b.append(" * @param roleList\n");
		b.append(" * @return true if user is supplied with one of given roles\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("if(roleList != null)\n");
		b.append(addRoleCheck());
		b.append("if(!sendError)\n");
		b.append("return false;\n\n");
		b.append("final FacesContext facesContext = FacesContext.getCurrentInstance();\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("final String errorMessage = ");
		b.append(i18n.getBundleFragment("msg_access_denied", "YOU ARE NOT AUTHORIZED TO OPEN SELECTED PAGE!") + ";\n\n");
		b.append("facesContext.getExternalContext().responseSendError(HttpServletResponse.SC_FORBIDDEN, errorMessage);\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Failed to send error code!", "e");

		b.append("}\n\n");
		b.append("facesContext.responseComplete();\n");
		b.append("return false;\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		if (clientDomainObject != null) {
			final DomainAttribute clientPkAttr = clientDomainObject.getPKAttribute();
			DTOBeanAttribute clientDTOAttr = null;
			var adminRoles = "";

			methodSignature = "boolean checkClient(" + clientPkAttr.getJavaType().getName() + " clientId)";

			for (final DTOBeanAttribute attr : logOnDTO.getAttributes()) {
				if (attr.getAssociation() == null || attr.getDomainAttribute() == null)
					continue;

				if (attr.getAssociation().getTarget().equals(clientDomainObject) && attr.getDomainAttribute().isPk()) {
					clientDTOAttr = attr;
					break;
				}
			}

			if (clientDTOAttr == null)
				throw new IllegalStateException("The client attribute could not be found!");

			b = new StringBuilder();
			b.append("/**\n");
			b.append(" * @param clientId\n");
			b.append(" * @return true if user is allowed to open a page containing an object that belongs to a given client\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + methodSignature + "\n");
			b.append("{\n");

			for (final Role role : project.getRoles()) {
				if (!role.isAdminRole())
					continue;

				final var roleName = "ROLE_" + role.getName().toUpperCase();

				if (adminRoles.isEmpty())
					adminRoles += "roleList.add(" + roleName + ");\n";
			}

			if (!adminRoles.isEmpty()) {
				b.append("final var roleList = new ArrayList<String>();\n");
				b.append(adminRoles);
				b.append("\n");
				b.append("// Administrators should be allowed to access all objects!\n");
				b.append(addRoleCheck());
			}

			if (!clientPkAttr.getJavaType().isPrimitive())
				b.append("if(clientId != null && ");
			else
				b.append("if(");

			b.append("clientId");

			if (clientPkAttr.getJavaType().isPrimitive())
				b.append(" == ");
			else
				b.append(".equals(");

			b.append("principal." + clientDTOAttr.getGetterName());

			if (!clientPkAttr.getJavaType().isPrimitive())
				b.append(")");

			b.append(")\n");
			b.append("return true;\n\n");
			b.append("final FacesContext facesContext = FacesContext.getCurrentInstance();\n\n");
			b.append("try\n");
			b.append("{\n");
			b.append("final String errorMessage = ");
			b.append(i18n.getBundleFragment("msg_access_denied", "YOU ARE NOT AUTHORIZED TO OPEN SELECTED PAGE!") + ";\n\n");
			b.append("facesContext.getExternalContext().responseSendError(HttpServletResponse.SC_FORBIDDEN, errorMessage);\n");
			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			addErrorLog(b, "Failed to send error code!", "e");

			b.append("}\n\n");
			b.append("facesContext.responseComplete();\n");
			b.append("return false;\n");
			b.append("}\n\n");

			addMethod(methodSignature, b.toString());
		}

		// The login() method of a Spring application must return the navigation target! In case of a Jakarta EE application it is
		// better to return void in order to avoid potential problems when rendering the initial view.
		if (project.isSpringBootApplication())
			methodSignature = "String login()";
		else
			methodSignature = "void login()";

		b = new StringBuilder();
		var userNameGetter = "";
		var passwordGetter = "";
		var passwordSetter = "";

		for (final DTOBeanAttribute attr : logOnDTO.getAttributes()) {
			if (attr.getDomainAttribute() == null)
				continue;

			if (attr.getDomainAttribute().getTag() == AttributeTagEnumeration.USER_NAME)
				userNameGetter = attr.getGetterName();
			else if (attr.getDomainAttribute().getTag() == AttributeTagEnumeration.USER_PASSWORD) {
				passwordGetter = attr.getGetterName();
				passwordSetter = attr.getSetterName();
			}
		}

		b.append("/**\n");
		b.append(" * Perform login operation\n");

		if (project.isSpringBootApplication())
			b.append(" * @return the navigation target\n");

		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");

		addDebugLog(b, "Login user '{}'", SESSION_OWNER + "." + userNameGetter);

		b.append("\n");

		if (project.isJakartaEEApplication()) {
			b.append("final var credential = new UsernamePasswordCredential(" + SESSION_OWNER);
			b.append("." + userNameGetter + ", new Password(" + SESSION_OWNER + "." + passwordGetter + "));\n");
		}

		b.append("final var resp = (HttpServletResponse) FacesContext.getCurrentInstance().getExternalContext().getResponse();\n");
		b.append("final var req = (HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRequest();\n");
		b.append("\n");
		b.append("try\n");
		b.append("{\n");
		b.append("final String enteredPassword = " + SESSION_OWNER + "." + passwordGetter + ";\n");
		b.append("final String encryptedPassword = HashGenerator.encryptSHA256(enteredPassword);\n\n");
		b.append("// Verify if user exists\n");
		b.append(SESSION_OWNER + " = ");

		new ServiceInvocationGenerator(logOnMethod, b).addInvocation(SESSION_OWNER + "." + userNameGetter, "encryptedPassword");

		b.append("\n");
		b.append("// Due to security reasons we don't want to carry the password along the hole session!\n");
		b.append(SESSION_OWNER + "." + passwordSetter + "(null);\n\n");

		if (project.isJakartaEEApplication()) {
			b.append("final AuthenticationStatus status = securityContext.authenticate(req, resp, ");
			b.append("AuthenticationParameters.withParams().credential(credential));\n\n");
			b.append("if(!status.equals(AuthenticationStatus.SEND_CONTINUE) && !status.equals(AuthenticationStatus.SUCCESS))\n");
			b.append("{\n");

			addInfoLog(b, "Login of user '{}' failed! Status: {}", SESSION_OWNER + "." + userNameGetter, "status");

			b.append("\n");
			b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, TranslationKeys.LOG_ON_FAILED);\n");
			b.append("return;\n");
			b.append("}\n\n");
		}
		else {
			b.append("final Authentication authenticate = authenticationManager.authenticate(new UsernamePasswordAuthenticationToken");
			b.append("(principal." + userNameGetter + ", encryptedPassword));\n\n");
			b.append("final SecurityContext securityContext = SecurityContextHolder.getContext();\n");
			b.append("securityContext.setAuthentication(authenticate);\n\n");
			b.append("// Save the security context in the current session\n");
			b.append("final HttpSession session = req.getSession(true);\n");
			b.append("session.setAttribute(HttpSessionSecurityContextRepository.SPRING_SECURITY_CONTEXT_KEY, securityContext);\n\n");
		}

		b.append("// Save entered user name in cookie\n");
		b.append("final var userNameCookie = new Cookie(USER_NAME_COOKIE, URLEncoder.encode(" + SESSION_OWNER);
		b.append("." + userNameGetter + ", StandardCharsets.UTF_8));\n");
		b.append("userNameCookie.setPath(req.getContextPath());\n\n");
		b.append("resp.addCookie(userNameCookie);\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Error while performing login of user '{}'!", "e", SESSION_OWNER + "." + userNameGetter);

		b.append("\n");
		b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, LOG_ON_FAILED, e);\n");

		if (project.isSpringBootApplication()) {
			b.append("return null;\n");
			b.append("}\n\n");
			b.append("return START_PAGE;\n");
		}
		else
			b.append("}\n");

		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "String changePassword()";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Method to change user password\n");
		b.append(" * @return the navigation target\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("if(!newPassword.equals(confirmedPassword))\n");
		b.append("{\n");
		b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_WARN, MSG_PASSWORDS_NOT_EQUAL);\n");
		b.append("return null;\n");
		b.append("}\n\n");
		b.append("try\n");
		b.append("{\n");

		final var params = new ArrayList<String>();
		params.add(SESSION_OWNER + "." + logOnDTO.getPKAttribute().getGetterName());
		params.add("oldPassword");
		params.add("newPassword");
		params.add("confirmedPassword");

		new ServiceInvocationGenerator(changePasswordMethod, b).addInvocation(params.stream().toArray(String[]::new));

		b.append("\n");
		b.append("return START_PAGE;\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Error while saving new password!", "e");

		b.append("\n");
		b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_WARN, OPERATION_SAVE_FAIL, e);\n");
		b.append("return null;\n");
		b.append("}\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "void setLastPage(String navigationCase)";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Save last visited URL in history\n");
		b.append(" * @param navigationCase\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("history.push(navigationCase);\n\n");
		b.append("if(history.size() > 20)\n");
		b.append("history.pollLast();\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "String getLastPage()";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * @return the last entry from history list. If list is empty an empty String will be returned.\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("if(!history.isEmpty())\n");
		b.append("return history.pop();\n\n");
		b.append("return \"\";\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "boolean redirectTo(String currentPageURL, String targetURL)";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * @param currentPageURL the relative URL of the current page\n");
		b.append(" * @param targetURL the relative URL of the page to redirect to\n");
		b.append(" * @return true if the method has redirected the request to the given target URL\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("final FacesContext context = FacesContext.getCurrentInstance();\n");
		b.append("final String contextPath = context.getExternalContext().getRequestContextPath();\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("if(targetURL != null && !targetURL.isEmpty())\n");
		b.append("{\n");
		b.append("setLastPage(currentPageURL);\n\n");
		b.append("context.getExternalContext().redirect(contextPath + targetURL);\n\n");

		addDebugLog(b, "Redirect to page '{}'", "targetURL");

		b.append("\n");
		b.append("return true;\n");
		b.append("}\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Could not redirect to URL '{}'!", "e", "targetURL");

		b.append("}\n\n");
		b.append("return false;\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		methodSignature = "String saveSettings()";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Save user settings in respective cookies\n");
		b.append(" * @return the navigation target\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public String saveSettings()\n");
		b.append("{\n");
		b.append("final var resp = (HttpServletResponse) FacesContext.getCurrentInstance().getExternalContext().getResponse();\n");
		b.append("final var req = (HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRequest();\n\n");
		b.append("try\n");
		b.append("{\n");
		b.append("final var dateFormatCookie = ");
		b.append("new Cookie(DATE_FORMAT_COOKIE, URLEncoder.encode(dateFormat, StandardCharsets.UTF_8));\n");
		b.append("dateFormatCookie.setPath(req.getContextPath());\n\n");
		b.append("final var dateTimeFormatCookie = ");
		b.append("new Cookie(DATE_TIME_FORMAT_COOKIE, URLEncoder.encode(dateTimeFormat, StandardCharsets.UTF_8));\n");
		b.append("dateTimeFormatCookie.setPath(req.getContextPath());\n\n");
		b.append("final var numberFormatCookie = ");
		b.append("new Cookie(NUMBER_FORMAT_COOKIE, URLEncoder.encode(numberFormat, StandardCharsets.UTF_8));\n");
		b.append("numberFormatCookie.setPath(req.getContextPath());\n\n");
		b.append("final var timezoneCookie = new Cookie(TIMEZONE_COOKIE, URLEncoder.encode(timeZone, StandardCharsets.UTF_8));\n");
		b.append("timezoneCookie.setPath(req.getContextPath());\n\n");
		b.append("resp.addCookie(dateFormatCookie);\n");
		b.append("resp.addCookie(dateTimeFormatCookie);\n");
		b.append("resp.addCookie(numberFormatCookie);\n");
		b.append("resp.addCookie(timezoneCookie);\n");
		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		addErrorLog(b, "Error while reading format preferences from cookies!", "e");

		b.append("\n");
		b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_SAVE_FAIL, e);\n");
		b.append("return null;\n");
		b.append("}\n\n");
		b.append("return START_PAGE;\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		i18n.save();
	}

	/**
	 * @return the generated content
	 */
	private String addRoleCheck() {
		final var b = new StringBuilder();
		b.append("for(final String appRole : roleList)\n");

		for (final DTOBeanAttribute attr : logOnDTO.getAttributes()) {
			if (attr.getAssociation() == null || attr.getAssociation().getTag() != AssociationTagEnumeration.USER_ROLE)
				continue;

			final String roleAttrGetter = attr.getGetterName();
			DTOBeanAttribute displayAttr = attr.getReferencedDTOBean().getDisplayAttribute();

			if (displayAttr == null)
				displayAttr = attr.getReferencedDTOBean().getPKAttribute();

			if (attr.getAssociation() instanceof ManyToManyAssociation) {
				b.append("for(final " + attr.getReferencedDTOBean().getName() + " role : ");
				b.append(SESSION_OWNER + "." + roleAttrGetter + ")\n");
				b.append("if(role");
			}
			else
				b.append("if(" + SESSION_OWNER + "." + roleAttrGetter);

			b.append("." + displayAttr.getGetterName() + ".equals(appRole))\n");
			b.append("return true;\n\n");
			break;
		}

		return b.toString();
	}

}
