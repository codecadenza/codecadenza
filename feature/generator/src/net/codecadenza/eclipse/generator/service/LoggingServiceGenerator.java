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
package net.codecadenza.eclipse.generator.service;

import static net.codecadenza.eclipse.shared.Constants.LOGGING_BEAN_NAME;
import static net.codecadenza.eclipse.shared.Constants.PACK_SERVICE;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_PERSIST;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_BEAN;

import java.util.EnumMap;
import java.util.Map;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.java.EnumLiteral;
import net.codecadenza.eclipse.model.java.EnumLiteralTagEnumeration;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for the implementation of the logging service
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class LoggingServiceGenerator extends AbstractJavaSourceGenerator {
	private static final int MAX_CONTENT_LENGTH = 1024;

	private final Project project;
	private final DomainObject loggingObject;
	private final Map<AttributeTagEnumeration, DomainAttribute> tagMapping = new EnumMap<>(AttributeTagEnumeration.class);
	private AbstractDomainAssociation userAssoc;
	private JavaEnum logLevelEnum;
	private String userNamedQueryName;
	private String userNamedParmName;

	/**
	 * Constructor
	 * @param project
	 */
	public LoggingServiceGenerator(Project project) {
		this.project = project;
		this.loggingObject = project.getDomainObjectByTag(DomainTagEnumeration.LOGGING);

		if (loggingObject == null)
			return;

		for (final DomainAttribute att : loggingObject.getAttributes())
			if (att.getTag() != AttributeTagEnumeration.NONE)
				this.tagMapping.put(att.getTag(), att);

		for (final Map.Entry<AttributeTagEnumeration, DomainAttribute> entry : tagMapping.entrySet())
			if (entry.getKey() == AttributeTagEnumeration.LOGGING_LEVEL) {
				final DomainAttribute attr = entry.getValue();
				this.logLevelEnum = (JavaEnum) attr.getJavaType();
			}

		for (final AbstractDomainAssociation assoc : loggingObject.getAssociations())
			if (assoc.getTag() == AssociationTagEnumeration.LOGGING_USER) {
				this.userAssoc = assoc;
				final DomainObject userObject = userAssoc.getTarget();

				if (userObject.getTag() != DomainTagEnumeration.USER)
					throw new IllegalStateException("The domain object that represents the user is not tagged properly!");

				for (final DomainAttribute attr : userObject.getAttributes())
					if (attr.getTag() == AttributeTagEnumeration.USER_NAME && attr.isDisplayAttribute()) {
						this.userNamedQueryName = "NQ_UK_FIND_BY_" + attr.getName().toUpperCase();
						this.userNamedParmName = attr.getName();
					}

				break;
			}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final String packageName = project.getRootNamespace().toString() + PACK_SERVICE + SUB_PACKAGE_BEAN;

		final var javaFile = new JavaFile(project, BuildArtifactType.SERVICE, LOGGING_BEAN_NAME, packageName);
		javaFile.setComment("Implementation of logging service");

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage("net.codecadenza.runtime.server.logging");

		if (project.isSpringBootApplication()) {
			importClass("org.springframework.stereotype.Service");
			importPackage("org.springframework.transaction.annotation");
		}
		else
			importPackage("jakarta.ejb");

		if (loggingObject == null)
			return;

		importPackage("jakarta.persistence");
		importPackage("java.util");
		importPackage(loggingObject.getNamespace().toString());

		// Add further imports that depend on the attributes of the logging domain object
		tagMapping.entrySet().forEach(entry -> {
			if (entry.getKey() == AttributeTagEnumeration.LOGGING_HOST)
				importPackage("java.net");
			else if (entry.getKey() == AttributeTagEnumeration.LOGGING_STACK_TRACE)
				importPackage("java.io");
			else if (entry.getKey() == AttributeTagEnumeration.LOGGING_LEVEL) {
				// It might be the case that the log level enumeration is in a different package!
				importPackage(entry.getValue().getJavaType().getNamespace().toString());
			}
		});

		if (userAssoc != null && userNamedQueryName != null)
			if (project.isJakartaEEApplication())
				importPackage("java.security");
			else {
				importPackage("org.springframework.security.core");
				importPackage("org.springframework.security.core.context");
			}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		if (project.isJakartaEEApplication()) {
			b.append("@Stateless\n");
			b.append("@Local(LoggingService.class)\n");
		}
		else
			b.append("@Service\n");

		b.append("public class " + LOGGING_BEAN_NAME + " implements LoggingService");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		if (loggingObject == null)
			return;

		tagMapping.keySet().forEach(tag -> {
			if (tag == AttributeTagEnumeration.LOGGING_HOST)
				addPrivateConstant(JavaType.STRING, "UNKNOWN_HOST", "\"UNKNOWN\"").create();

			if (tag == AttributeTagEnumeration.LOGGING_STACK_TRACE) {
				Integer maxLength = tagMapping.get(tag).getDomainAttributeValidator().getMaxLength();

				if (maxLength == null)
					maxLength = MAX_CONTENT_LENGTH;

				addPrivateConstant(JavaType.INT, "MAX_STACK_TRACE_LENGTH", Integer.toString(maxLength)).create();
			}

			if (tag == AttributeTagEnumeration.LOGGING_MESSAGE) {
				Integer maxLength = tagMapping.get(tag).getDomainAttributeValidator().getMaxLength();

				if (maxLength == null)
					maxLength = MAX_CONTENT_LENGTH;

				addPrivateConstant(JavaType.INT, "MAX_MESSAGE_LENGTH", Integer.toString(maxLength)).create();
			}
		});

		addPrivateField("EntityManager", "em").withAnnotations("@PersistenceContext\n").create();

		if (userAssoc != null && project.isJakartaEEApplication())
			addPrivateField("SessionContext", "sessionContext").create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		var b = new StringBuilder();

		if (userAssoc != null && project.isJakartaEEApplication())
			addResourceSetterMethod("SessionContext", "sessionContext");

		if (loggingObject == null) {
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.server.logging.LoggingService#");
			b.append("debug(net.codecadenza.runtime.server.logging.LoggingDTO)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public void debug(LoggingDTO logEntry)\n");
			b.append("{\n");
			b.append("// No implementation required!\n");
			b.append("}\n\n");

			addMethod("void debug(LoggingDTO logEntry)", b.toString());

			b = new StringBuilder();
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.server.logging.LoggingService#");
			b.append("info(net.codecadenza.runtime.server.logging.LoggingDTO)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public void info(LoggingDTO logEntry)\n");
			b.append("{\n");
			b.append("// No implementation required!\n");
			b.append("}\n\n");

			addMethod("void info(LoggingDTO logEntry)", b.toString());

			b = new StringBuilder();
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.server.logging.LoggingService#");
			b.append("warn(net.codecadenza.runtime.server.logging.LoggingDTO)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public void warn(LoggingDTO logEntry)\n");
			b.append("{\n");
			b.append("// No implementation required!\n");
			b.append("}\n\n");

			addMethod("void warn(LoggingDTO logEntry)", b.toString());

			b = new StringBuilder();
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.server.logging.LoggingService#");
			b.append("error(net.codecadenza.runtime.server.logging.LoggingDTO)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public void error(LoggingDTO logEntry)\n");
			b.append("{\n");
			b.append("// No implementation required!\n");
			b.append("}\n\n");

			addMethod("void error(LoggingDTO logEntry)", b.toString());

			return;
		}

		b.append("/**\n");
		b.append(" * Save log entry\n");
		b.append(" * @param logEntry\n");
		b.append(" * @param logLevel\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private void saveLogEntry(LoggingDTO logEntry, " + logLevelEnum.getName() + " logLevel)\n");
		b.append("{\n");
		b.append("final var log = new " + loggingObject.getName() + "();\n");

		for (final Map.Entry<AttributeTagEnumeration, DomainAttribute> entry : tagMapping.entrySet()) {
			final DomainAttribute attr = entry.getValue();
			final String setter = attr.getSetterName();

			if (entry.getKey() == AttributeTagEnumeration.LOGGING_CLASS_NAME)
				b.append("log." + setter + "(logEntry.getClassName());\n");
			else if (entry.getKey() == AttributeTagEnumeration.LOGGING_DATE) {
				if (attr.getJavaType().isDate())
					b.append("log." + setter + "(new Date());\n");
				else
					b.append("log." + setter + "(new GregorianCalendar());\n");
			}
			else if (entry.getKey() == AttributeTagEnumeration.LOGGING_DURATION)
				b.append("log." + setter + "(logEntry.getDuration());\n");
			else if (entry.getKey() == AttributeTagEnumeration.LOGGING_METHOD_NAME)
				b.append("log." + setter + "(logEntry.getMethodName());\n");
			else if (entry.getKey() == AttributeTagEnumeration.LOGGING_LEVEL)
				b.append("log." + setter + "(logLevel);\n");
			else if (entry.getKey() == AttributeTagEnumeration.LOGGING_MESSAGE) {
				b.append("\nif(logEntry.getMessage() != null)\n");
				b.append("if(logEntry.getMessage().length() > MAX_MESSAGE_LENGTH)\n");
				b.append("log." + setter + "(logEntry.getMessage().substring(0, MAX_MESSAGE_LENGTH - 1));\n");
				b.append("else\n");
				b.append("log." + setter + "(logEntry.getMessage());\n\n");
			}
			else if (entry.getKey() == AttributeTagEnumeration.LOGGING_HOST) {
				b.append("\ntry\n");
				b.append("{\n");
				b.append("log." + setter + "(InetAddress.getLocalHost().getHostName());\n");
				b.append("}\n");
				b.append("catch (final UnknownHostException e)\n");
				b.append("{\n");
				b.append("log." + setter + "(UNKNOWN_HOST);\n");
				b.append("}\n\n");
			}
			else if (entry.getKey() == AttributeTagEnumeration.LOGGING_STACK_TRACE) {
				b.append("\nif(logEntry.getThrowable() != null)\n");
				b.append("{\n");
				b.append("final var stackTraceWriter = new StringWriter();\n");
				b.append("logEntry.getThrowable().printStackTrace(new PrintWriter(stackTraceWriter));\n\n");
				b.append("if(stackTraceWriter.toString().length() > MAX_STACK_TRACE_LENGTH)\n");
				b.append("log." + setter + "(stackTraceWriter.toString().substring(0, MAX_STACK_TRACE_LENGTH - 1));\n");
				b.append("else\n");
				b.append("log." + setter + "(stackTraceWriter.toString());\n");
				b.append("}\n\n");
			}
		}

		if (userAssoc != null && userNamedQueryName != null) {
			final String setter = userAssoc.getSetterName();
			final DomainObject userObj = userAssoc.getTarget();

			b.append("\ntry\n");
			b.append("{\n");

			if (project.isJakartaEEApplication()) {
				b.append("final Principal caller = sessionContext.getCallerPrincipal();\n");
				b.append("final String userName = caller.getName();\n\n");
			}
			else {
				b.append("org.springframework.security.core.userdetails.User userDetails = null;\n");
				b.append("final Authentication authentication = SecurityContextHolder.getContext().getAuthentication();\n");
				b.append("userDetails = (org.springframework.security.core.userdetails.User) authentication.getPrincipal();\n");
				b.append("final String userName = userDetails.getUsername();\n\n");
			}

			b.append("if(userName != null)\n");
			b.append("{\n");
			b.append("final TypedQuery<" + userObj.getName() + "> query = em.createNamedQuery(");
			b.append(userObj.getName() + "." + userNamedQueryName + ", " + userObj.getName() + ".class);\n");
			b.append("query.setParameter(\"" + userNamedParmName + "\", userName);\n\n");
			b.append("log." + setter + "(query.getResultList().stream().findFirst().orElse(null));\n");
			b.append("}\n");
			b.append("}\n");
			b.append("catch (final Exception ex)\n");
			b.append("{\n");
			b.append("// It might be the case that no principal is bound to session context!\n");
			b.append("}\n\n");
		}

		b.append("em." + REPO_METHOD_NAME_PERSIST + "(log);\n");
		b.append("}\n\n");

		addMethod("void saveLogEntry(LoggingDTO logEntry, " + logLevelEnum.getName() + " logLevel)", b.toString());

		// Implement all necessary logging methods
		for (final EnumLiteral literal : logLevelEnum.getEnumerationValues()) {
			if (literal.getTag() == EnumLiteralTagEnumeration.LOGGING_LEVEL_DEBUG) {
				b = new StringBuilder();
				b.append("/* (non-Javadoc)\n");
				b.append(" * @see net.codecadenza.runtime.server.logging.LoggingService#");
				b.append("debug(net.codecadenza.runtime.server.logging.LoggingDTO)\n");
				b.append(" */\n");
				b.append("@Override\n");
				b.append(getAnnotationForGeneratedElement());

				if (project.isJakartaEEApplication())
					b.append("@TransactionAttribute(TransactionAttributeType.REQUIRES_NEW)\n");
				else
					b.append("@Transactional(propagation = Propagation.REQUIRES_NEW)\n");

				b.append("public void debug(LoggingDTO logEntry)\n");
				b.append("{\n");
				b.append("saveLogEntry(logEntry, " + logLevelEnum.getName() + "." + literal.getName() + ");\n");
				b.append("}\n\n");

				addMethod("void debug(LoggingDTO logEntry)", b.toString());
			}
			else if (literal.getTag() == EnumLiteralTagEnumeration.LOGGING_LEVEL_INFO) {
				b = new StringBuilder();
				b.append("/* (non-Javadoc)\n");
				b.append(" * @see net.codecadenza.runtime.server.logging.LoggingService#");
				b.append("info(net.codecadenza.runtime.server.logging.LoggingDTO)\n");
				b.append(" */\n");
				b.append("@Override\n");
				b.append(getAnnotationForGeneratedElement());

				if (project.isJakartaEEApplication())
					b.append("@TransactionAttribute(TransactionAttributeType.REQUIRES_NEW)\n");
				else
					b.append("@Transactional(propagation = Propagation.REQUIRES_NEW)\n");

				b.append("public void info(LoggingDTO logEntry)\n");
				b.append("{\n");
				b.append("saveLogEntry(logEntry, " + logLevelEnum.getName() + "." + literal.getName() + ");\n");
				b.append("}\n\n");

				addMethod("void info(LoggingDTO logEntry)", b.toString());
			}
			else if (literal.getTag() == EnumLiteralTagEnumeration.LOGGING_LEVEL_WARN) {
				b = new StringBuilder();
				b.append("/* (non-Javadoc)\n");
				b.append(" * @see net.codecadenza.runtime.server.logging.LoggingService#");
				b.append("warn(net.codecadenza.runtime.server.logging.LoggingDTO)\n");
				b.append(" */\n");
				b.append("@Override\n");
				b.append(getAnnotationForGeneratedElement());

				if (project.isJakartaEEApplication())
					b.append("@TransactionAttribute(TransactionAttributeType.REQUIRES_NEW)\n");
				else
					b.append("@Transactional(propagation = Propagation.REQUIRES_NEW)\n");

				b.append("public void warn(LoggingDTO logEntry)\n");
				b.append("{\n");
				b.append("saveLogEntry(logEntry, " + logLevelEnum.getName() + "." + literal.getName() + ");\n");
				b.append("}\n\n");

				addMethod("void warn(LoggingDTO logEntry)", b.toString());
			}
			else if (literal.getTag() == EnumLiteralTagEnumeration.LOGGING_LEVEL_ERROR) {
				b = new StringBuilder();
				b.append("/* (non-Javadoc)\n");
				b.append(" * @see net.codecadenza.runtime.server.logging.LoggingService#");
				b.append("error(net.codecadenza.runtime.server.logging.LoggingDTO)\n");
				b.append(" */\n");
				b.append("@Override\n");
				b.append(getAnnotationForGeneratedElement());

				if (project.isJakartaEEApplication())
					b.append("@TransactionAttribute(TransactionAttributeType.REQUIRES_NEW)\n");
				else
					b.append("@Transactional(propagation = Propagation.REQUIRES_NEW)\n");

				b.append("public void error(LoggingDTO logEntry)\n");
				b.append("{\n");
				b.append("saveLogEntry(logEntry, " + logLevelEnum.getName() + "." + literal.getName() + ");\n");
				b.append("}\n\n");

				addMethod("void error(LoggingDTO logEntry)", b.toString());
			}
		}
	}

}
