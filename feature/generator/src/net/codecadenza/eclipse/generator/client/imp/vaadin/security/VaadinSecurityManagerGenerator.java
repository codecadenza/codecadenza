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
package net.codecadenza.eclipse.generator.client.imp.vaadin.security;

import static net.codecadenza.eclipse.generator.basic.client.imp.AbstractClientProjectFilesGenerator.SECURITY_MANAGER_COMMENT;
import static net.codecadenza.eclipse.shared.Constants.APP_LOGON_DTO_NAME;
import static net.codecadenza.eclipse.shared.Constants.SECURITY_MANAGER;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_UTIL;

import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for the security manager of a Vaadin application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class VaadinSecurityManagerGenerator extends AbstractJavaSourceGenerator {
	private final Project project;
	private final DTOBean logOnDTO;
	private final BoundaryBean logOnBoundary;

	/**
	 * Constructor
	 * @param project
	 */
	public VaadinSecurityManagerGenerator(Project project) {
		this.project = project;
		this.logOnDTO = project.getApplicationLogOnDTO();
		this.logOnBoundary = project.getLogOnBoundary();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final String packageName = project.getClientNamespace().toString() + SUB_PACKAGE_UTIL;

		final var javaFile = new JavaFile(project, BuildArtifactType.GUI, SECURITY_MANAGER, packageName);
		javaFile.setComment(SECURITY_MANAGER_COMMENT);

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage("java.io");
		importClass("java.util.HashSet");
		importPackage("net.codecadenza.runtime.crypto");
		importPackage("com.vaadin.flow.server");
		importPackage(logOnDTO.getNamespace().toString());

		if (project.isSpringBootApplication()) {
			importPackage("org.springframework.security.authentication");
			importPackage("org.springframework.security.core");
			importPackage("org.springframework.security.core.context");
			importClass("org.springframework.security.web.context.HttpSessionSecurityContextRepository");
			importClass("org.springframework.stereotype.Service");
			importClass("org.springframework.web.context.annotation.SessionScope");
		}
		else {
			importPackage("jakarta.enterprise.context");
			importPackage("jakarta.servlet.http");
			importPackage("jakarta.security.enterprise.credential");
			importPackage("jakarta.security.enterprise.authentication.mechanism.http");
			importPackage("jakarta.security.enterprise");
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		if (project.isSpringBootApplication()) {
			b.append("@Service\n");
			b.append("@SessionScope\n");
		}
		else
			b.append("@SessionScoped\n");

		b.append("public class " + SECURITY_MANAGER + " implements Serializable");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		project.getRoles().forEach(role -> addPublicConstant(JavaType.STRING, "ROLE_" + role.getName().toUpperCase(),
				"\"" + role.getName().toUpperCase() + "\"").create());

		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();

		new ServiceDeclarationGenerator(this, logOnBoundary).addField();

		addPrivateField(APP_LOGON_DTO_NAME, "logOnDTO").create();
		addPrivateField("HashSet<String>", "roleSet").withDefaultValue("new HashSet<>()").withFinalModifier().create();

		if (project.isSpringBootApplication())
			addPrivateField("AuthenticationManager", "authenticationManager").withTransientModifier().withFinalModifier().inject()
					.create();
		else
			addPrivateField("SecurityContext", "securityContext").withTransientModifier().withFinalModifier().inject().create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final BoundaryMethod logOnMethod = logOnBoundary.getBoundaryMethodByReturnType(logOnDTO,
				BoundaryMethodTypeEnumeration.LOG_ON);

		var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * @return the log-on DTO\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + APP_LOGON_DTO_NAME + " getLogOnDTO()\n");
		b.append("{\n");
		b.append("return logOnDTO;\n");
		b.append("}\n\n");

		addMethod(APP_LOGON_DTO_NAME + " getLogOnDTO()", b.toString());

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Log-on user\n");
		b.append(" * @param userName\n");
		b.append(" * @param password\n");
		b.append(" * @throws Exception if log-on has failed\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public void logOn(String userName, String password) throws Exception\n");
		b.append("{\n");

		if (project.isSpringBootApplication()) {
			b.append("final Authentication authenticate = authenticationManager.authenticate(new UsernamePasswordAuthenticationToken");
			b.append("(userName, HashGenerator.encryptSHA256(password)));\n\n");
			b.append("final SecurityContext securityContext = SecurityContextHolder.getContext();\n");
			b.append("securityContext.setAuthentication(authenticate);\n\n");
			b.append("// Save the security context in the current session\n");
			b.append("final WrappedSession session = VaadinService.getCurrentRequest().getWrappedSession();\n");
			b.append("session.setAttribute(HttpSessionSecurityContextRepository.SPRING_SECURITY_CONTEXT_KEY, securityContext);\n\n");
		}
		else {
			b.append("final HttpServletResponse response = VaadinServletService.getCurrentResponse();\n");
			b.append("final HttpServletRequest request = VaadinServletService.getCurrentServletRequest();\n");
			b.append("final var credential = new UsernamePasswordCredential(userName, new Password(password));\n");
			b.append("final AuthenticationStatus status = securityContext.authenticate(request, response, ");
			b.append("AuthenticationParameters.withParams().credential(credential));\n\n");

			addDebugLog(b, "Authentication status: {}", "status");

			b.append("\n");
			b.append("if(!status.equals(AuthenticationStatus.SEND_CONTINUE) && !status.equals(AuthenticationStatus.SUCCESS))\n");
			b.append("throw new IllegalStateException(\"Login failed!\");\n\n");
		}

		b.append("// Perform application login\n");
		b.append("logOnDTO = ");

		new ServiceInvocationGenerator(logOnMethod, b).addInvocation("userName", "HashGenerator.encryptSHA256(password)");

		b.append("\n");

		var roleAttrName = "";

		for (final DTOBeanAttribute attr : logOnDTO.getAttributes()) {
			if (attr.getAssociation() == null)
				continue;

			if (attr.getAssociation().getTag() != AssociationTagEnumeration.USER_ROLE)
				continue;

			roleAttrName = attr.getGetterName();

			if (attr.getAssociation() instanceof ManyToManyAssociation) {
				b.append("for(final " + attr.getReferencedDTOBean().getName() + " role : logOnDTO." + roleAttrName + ")\n");

				final DTOBeanAttribute displayAttr = attr.getReferencedDTOBean().getDisplayAttribute();

				if (displayAttr != null) {
					final String roleAttrName2 = displayAttr.getDomainAttribute().getGetterName();

					b.append("roleSet.add(role." + roleAttrName2 + ");\n");
				}
				else {
					final DTOBeanAttribute pkAttr = attr.getReferencedDTOBean().getPKAttribute();

					if (pkAttr.getDomainAttribute().getJavaType().isString()) {
						final String roleAttrName2 = pkAttr.getDomainAttribute().getGetterName();

						b.append("roleSet.add(role." + roleAttrName2 + ");\n");
					}
				}
			}
			else if (attr.getAssociation() instanceof ManyToOneAssociation) {
				final DTOBeanAttribute displayAttr = attr.getReferencedDTOBean().getDisplayAttribute();

				if (displayAttr != null) {
					final String roleAttrName2 = displayAttr.getDomainAttribute().getGetterName();

					b.append("roleSet.add(logOnDTO." + roleAttrName + "." + roleAttrName2 + ");\n");
				}
				else {
					final DTOBeanAttribute pkAttr = attr.getReferencedDTOBean().getPKAttribute();

					if (pkAttr.getDomainAttribute().getJavaType().isString()) {
						final String roleAttrName2 = pkAttr.getDomainAttribute().getGetterName();

						b.append("roleSet.add(logOnDTO." + roleAttrName + "." + roleAttrName2 + ");\n");
					}
				}
			}

			break;
		}

		b.append("}\n\n");

		addMethod("void logOn(String userName, String password)", b.toString());

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Check if at least one of given roles is granted to user\n");
		b.append(" * @param roleNames a variable argument list of role names\n");
		b.append(" * @return true if authorization is permitted\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public boolean checkAuthorization(String... roleNames)\n");
		b.append("{\n");
		b.append("if(roleNames == null)\n");
		b.append("return false;\n\n");
		b.append("for(final String roleName : roleNames)\n");
		b.append("if(roleSet.contains(roleName))\n");
		b.append("return true;\n\n");
		b.append("return false;\n");
		b.append("}\n\n");

		addMethod("boolean checkAuthorization(String... roleNames)", b.toString());

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * @return true if user is logged in\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public boolean isLoggedIn()\n");
		b.append("{\n");
		b.append("return logOnDTO != null;\n");
		b.append("}\n\n");

		addMethod("boolean isLoggedIn()", b.toString());

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Perform log out operation\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public void logOut()\n");
		b.append("{\n");
		b.append("roleSet.clear();\n\n");
		b.append("logOnDTO = null;\n");
		b.append("}\n\n");

		addMethod("void logOut()", b.toString());
	}

}
