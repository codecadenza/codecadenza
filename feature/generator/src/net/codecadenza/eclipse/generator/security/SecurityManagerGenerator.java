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
package net.codecadenza.eclipse.generator.security;

import static net.codecadenza.eclipse.generator.basic.client.imp.AbstractClientProjectFilesGenerator.SECURITY_MANAGER_COMMENT;
import static net.codecadenza.eclipse.shared.Constants.APP_LOGON_DTO_NAME;
import static net.codecadenza.eclipse.shared.Constants.APP_LOGON_EXCEPTION_NAME;
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
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for the security manager of a rich-client application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SecurityManagerGenerator extends AbstractJavaSourceGenerator {
	private final Project project;
	private final BoundaryBean logOnBoundary;
	private final DTOBean logOnDTO;

	/**
	 * Constructor
	 * @param project
	 */
	public SecurityManagerGenerator(Project project) {
		this.project = project;
		this.logOnBoundary = project.getLogOnBoundary();
		this.logOnDTO = project.getApplicationLogOnDTO();
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
		importClass("java.util.HashSet");
		importClass("java.util.Set");
		importPackage("net.codecadenza.runtime.crypto");
		importPackage("net.codecadenza.runtime.richclient.transport");

		if (project.hasRAPClient())
			importClass("org.eclipse.rap.rwt.SingletonUtil");

		importPackage(logOnDTO.getNamespace().toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public class " + SECURITY_MANAGER);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPrivateField(APP_LOGON_DTO_NAME, "logOnDTO").create();
		addPrivateField("ServiceLocatorDTO", "serviceLocatorDTO").create();
		addPrivateField("Set<String>", "roleSet").withDefaultValue("new HashSet<>()").withFinalModifier().create();

		if (project.hasRCPClient() || project.hasSwingClient() || project.hasJavaFXClient())
			addPrivateField(SECURITY_MANAGER, "singleton").withStaticModifier().create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		final var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Private constructor\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private " + SECURITY_MANAGER + "()\n");
		b.append("{\n\n");
		b.append("}\n\n");

		addConstructor(SECURITY_MANAGER + "()", b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final BoundaryMethod method = logOnBoundary.getBoundaryMethodByReturnType(logOnDTO, BoundaryMethodTypeEnumeration.LOG_ON);

		var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * @return the singleton instance\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public static synchronized " + SECURITY_MANAGER + " getInstance()\n");
		b.append("{\n");

		if (project.hasRCPClient() || project.hasSwingClient() || project.hasJavaFXClient()) {
			b.append("if(singleton == null)\n");
			b.append("singleton = new " + SECURITY_MANAGER + "();\n\n");
			b.append("return singleton;\n");
		}
		else
			b.append("return SingletonUtil.getSessionInstance(" + SECURITY_MANAGER + ".class);\n");

		b.append("}\n\n");

		addMethod(SECURITY_MANAGER + " + getInstance()", b.toString());

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * @return the log-on DTO\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public static " + APP_LOGON_DTO_NAME + " getLogOnDTO()\n");
		b.append("{\n");
		b.append("return getInstance().logOnDTO;\n");
		b.append("}\n\n");

		addMethod(APP_LOGON_DTO_NAME + " getLogOnDTO()", b.toString());

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * @return the host name\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public static String getHostName()\n");
		b.append("{\n");
		b.append("return getInstance().serviceLocatorDTO.getAlias();\n");
		b.append("}\n\n");

		addMethod("String getHostName()", b.toString());

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * @return the connection URL\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public static String getConnectionURL()\n");
		b.append("{\n");
		b.append("return getInstance().serviceLocatorDTO.getConnectionURL();\n");
		b.append("}\n\n");

		addMethod("String getConnectionURL()", b.toString());

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Log-on user\n");
		b.append(" * @param serviceLocatorDTO\n");
		b.append(" * @throws " + APP_LOGON_EXCEPTION_NAME + "\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public static synchronized void logOn(ServiceLocatorDTO serviceLocatorDTO)\n");
		b.append("{\n");

		if (!project.isJavaSEApplication()) {
			b.append("getInstance().serviceLocatorDTO = serviceLocatorDTO;\n\n");
			b.append("// Initialize service locator in order to communicate with application server\n");
		}

		b.append("ServiceLocator.initialize(serviceLocatorDTO);\n\n");

		final var declarationGenerator = new ServiceDeclarationGenerator(this, logOnBoundary, b);
		declarationGenerator.addLocalVariable();

		b.append("\n");
		b.append("try\n");
		b.append("{\n");
		b.append("getInstance().logOnDTO = ");

		new ServiceInvocationGenerator(method, b).addInvocation("serviceLocatorDTO.getUserName()",
				"HashGenerator.encryptSHA256(serviceLocatorDTO.getPassword())");

		b.append("}\n");
		b.append("catch (final java.security.NoSuchAlgorithmException e)\n");
		b.append("{\n");
		b.append("throw new IllegalStateException(e);\n");
		b.append("}\n");

		declarationGenerator.addCloseStatementInFinallyBlock();

		b.append("\n");

		var roleAttrName = "";

		for (final DTOBeanAttribute attr : logOnDTO.getAttributes()) {
			if (attr.getAssociation() == null)
				continue;

			if (attr.getAssociation().getTag() != AssociationTagEnumeration.USER_ROLE)
				continue;

			roleAttrName = attr.getGetterName();

			if (attr.getAssociation() instanceof ManyToManyAssociation) {
				importPackage(attr.getReferencedDTOBean().getNamespace().toString());

				b.append("for(final " + attr.getReferencedDTOBean().getName() + " role : getInstance().logOnDTO." + roleAttrName + ")\n");

				final DTOBeanAttribute displayAttr = attr.getReferencedDTOBean().getDisplayAttribute();

				if (displayAttr != null) {
					final String roleAttrName2 = displayAttr.getDomainAttribute().getGetterName();

					b.append("getInstance().roleSet.add(role." + roleAttrName2 + ");\n");
				}
				else {
					final DTOBeanAttribute pkAttr = attr.getReferencedDTOBean().getPKAttribute();

					if (pkAttr.getDomainAttribute().getJavaType().isString()) {
						final String roleAttrName2 = pkAttr.getDomainAttribute().getGetterName();

						b.append("getInstance().roleSet.add(role." + roleAttrName2 + ");\n");
					}
				}
			}
			else if (attr.getAssociation() instanceof ManyToOneAssociation) {
				final DTOBeanAttribute displayAttr = attr.getReferencedDTOBean().getDisplayAttribute();

				if (displayAttr != null) {
					final String roleAttrName2 = displayAttr.getDomainAttribute().getGetterName();

					b.append("getInstance().roleSet.add(getInstance().logOnDTO." + roleAttrName + "." + roleAttrName2 + ");\n");
				}
				else {
					final DTOBeanAttribute pkAttr = attr.getReferencedDTOBean().getPKAttribute();

					if (pkAttr.getDomainAttribute().getJavaType().isString()) {
						final String roleAttrName2 = pkAttr.getDomainAttribute().getGetterName();

						b.append("getInstance().roleSet.add(getInstance().logOnDTO." + roleAttrName + "." + roleAttrName2 + ");\n");
					}
				}
			}

			break;
		}

		b.append("}\n\n");

		addMethod("boolean logOn(ServiceLocatorDTO serviceLocatorDTO)", b.toString());

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Check if at least one of given roles is granted to user\n");
		b.append(" * @param roleNames a variable argument list of role names\n");
		b.append(" * @return true if authorization is permitted\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public static boolean checkAuthorization(String... roleNames)\n");
		b.append("{\n");
		b.append("for(final String roleName : roleNames)\n");
		b.append("if(getInstance().roleSet.contains(roleName))\n");
		b.append("return true;\n\n");
		b.append("return false;\n");
		b.append("}\n\n");

		addMethod("boolean checkAuthorization(String... roleNames)", b.toString());
	}

}
