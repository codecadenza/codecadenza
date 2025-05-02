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
package net.codecadenza.eclipse.generator.exchange.method;

import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME_FORMAT;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethodTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.exchange.DirectExchangeMode;
import net.codecadenza.eclipse.model.exchange.FileExchangeMode;
import net.codecadenza.eclipse.model.exchange.StringExchangeMode;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.Role;
import net.codecadenza.eclipse.model.repository.PermissionModeEnumeration;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.service.AsynchronousInvocation;
import net.codecadenza.eclipse.model.service.ScheduledInvocation;

/**
 * <p>
 * Abstract base class for data exchange method generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractExchangeMethodGenerator {
	public static final String TRANSACTION_OBJ_NAME = "transaction";
	private static final Pattern REPLACEMENT_PATTERN = Pattern.compile("\\(");

	protected static final String DEFAULT_DOMAIN_OBJECT_NAME = "obj";
	protected static final String DEFAULT_OBJECT_LIST_NAME = "objectList";
	protected static final String DEFAULT_LIST_ITEM_NAME = "listEntry";
	protected static final String FILE_NAME_DATE_ELEMENT = "date";
	protected static final String FILE_NAME_ATTR_ELEMENT = "attr";
	protected static final String FILE_NAME_BATCHINDEX_ELEMENT = "batchindex";
	protected static final char DYNAMIC_FILE_NAME_ELEMENT_START = '{';
	protected static final char DYNAMIC_FILE_NAME_ELEMENT_END = '}';
	protected static final char FILE_NAME_PARAM_DELIMITER = ',';
	protected static final char FILE_NAME_FUNCTION_START = '(';
	protected static final char FILE_NAME_FUNCTION_END = ')';
	protected static final String CURRENT_DATE = "now";
	protected static final String FILE_NAME = "fileName";
	protected static final String BATCH_COUNTER = "batchCounter";
	protected static final String BATCH_INDEX = "batchIndex";
	protected static final String REPOSITORY_SUFFIX = "Repository";
	protected static final String FACADE_SUFFIX = "Facade";
	protected static final String DEFAULT_LIST_NAME = "items";
	protected static final String DEFAULT_PARENT_OBJ_NAME = "parentObject";
	protected static final String DEFAULT_PARENT_LIST_NAME = "parentObjList";

	protected final AbstractJavaSourceGenerator generator;
	protected DataExchangeMethod method;
	protected Project project;
	protected boolean stringExchangeMode;
	protected boolean directExchangeMode;
	protected DataExchangeServiceBean exchangeBean;
	protected FileExchangeMode fileExchangeMode;
	protected Map<String, Repository> repositories = new HashMap<>();
	protected Set<String> methodAssocSet = new HashSet<>();

	/**
	 * Constructor
	 * @param generator
	 * @param method
	 */
	protected AbstractExchangeMethodGenerator(AbstractJavaSourceGenerator generator, DataExchangeMethod method) {
		this.generator = generator;
		this.method = method;
		this.project = method.getDataExchangeServiceBean().getNamespace().getProject();
		this.exchangeBean = method.getDataExchangeServiceBean();

		if (method.getExchangeMode() instanceof StringExchangeMode)
			this.stringExchangeMode = true;
		else if (method.getExchangeMode() instanceof DirectExchangeMode)
			this.directExchangeMode = true;
		else if (method.getExchangeMode() instanceof final FileExchangeMode exchangeMode)
			this.fileExchangeMode = exchangeMode;
	}

	/**
	 * Create the method comment
	 * @return the generated comment
	 */
	public abstract String createMethodComment();

	/**
	 * @return the generated content
	 */
	protected abstract String createMethodForSingleObject();

	/**
	 * @return the generated content
	 */
	protected abstract String createMethodForMultipleObjects();

	/**
	 * Create the data exchange method
	 */
	public abstract void createMethod();

	/**
	 * Create additional methods that are necessary for this data exchange operation
	 */
	public abstract void createAdditionalMethods();

	/**
	 * @return a map containing all additional repositories necessary for this method
	 */
	public Map<String, Repository> getRepositories() {
		// We first have to call all generator methods in order to fill the hash set containing necessary configuration data properly!
		createMethod();

		createAdditionalMethods();

		return repositories;
	}

	/**
	 * @param repositories
	 */
	public void setRepositories(Map<String, Repository> repositories) {
		this.repositories = repositories;
	}

	/**
	 * @param domainObject
	 * @return the default repository name for a given domain object
	 */
	public String createRepositoryName(DomainObject domainObject) {
		if (project.isBoundaryMode())
			return domainObject.getLowerCaseName() + REPOSITORY_SUFFIX;

		return domainObject.getLowerCaseName() + FACADE_SUFFIX;
	}

	/**
	 * @param domainObject
	 * @param methodType
	 * @return an output message that can be added to the generated source file if a necessary repository method is missing
	 */
	protected String createOutputForMissingRepositoryMethod(DomainObject domainObject, RepositoryMethodTypeEnumeration methodType) {
		final var b = new StringBuilder();
		b.append("//-----------------------------------GENERATOR WARNING--------------------------------------\n");
		b.append("//\n");
		b.append("// Cannot complete source code generation as either the repository or a repository method\n");
		b.append("// of type \"" + methodType.getName() + "\" for domain object \"" + domainObject.getName() + "\" is missing!\n");
		b.append("//\n");
		b.append("//------------------------------------------------------------------------------------------\n");

		return b.toString();
	}

	/**
	 * Add all necessary imports
	 */
	public void addImports() {
		if (generator == null)
			return;

		generator.importPackage("net.codecadenza.runtime.exchange");

		if (method.getReturnType() != null && !method.getReturnType().isPrimitive() && method.getReturnType().getNamespace() != null)
			generator.importPackage(method.getReturnType().getNamespace().toString());

		for (final MethodParameter param : method.getMethodParameters())
			if (!param.getType().isPrimitive() && param.getType().getNamespace() != null)
				generator.importPackage(param.getType().getNamespace().toString());

		if (fileExchangeMode != null)
			generator.importPackage("java.io");

		for (final DataExchangeElement element : method.getRootElement(true).getAllElements()) {
			if (!element.isContainer())
				continue;

			if (element.getMappingObject() == null)
				continue;

			generator.importPackage(element.getMappingObject().getNamespace().toString());
		}

		if (project.isSpringBootApplication())
			if (method.getMethodInvocation() != null)
				generator.importPackage("org.springframework.scheduling.annotation");
			else
				generator.importPackage("jakarta.annotation.security");

		repositories.values().forEach(repository -> {
			if (!project.isBoundaryMode()) {
				final BoundaryBean boundary = project.getBoundaryByDomainObject(repository.getDomainObject());

				if (boundary != null)
					generator.importPackage(boundary.getNamespace().toString());
			}
			else
				generator.importPackage(repository.getNamespace().toString());
		});
	}

	/**
	 * @return the generated content
	 */
	protected String createInvocationDelayFragment() {
		final var b = new StringBuilder();

		if (project.isJavaSEApplication())
			return b.toString();

		if (!(method.getMethodInvocation() instanceof final AsynchronousInvocation asyncInvoc))
			return b.toString();

		if (asyncInvoc.getDelayInMilliseconds() != null && asyncInvoc.getDelayInMilliseconds() > 0) {
			b.append("// Please remove this code if you want to implement an invocation delay differently!\n");
			b.append("try\n");
			b.append("{\n");
			b.append("Thread.sleep(" + asyncInvoc.getDelayInMilliseconds() + ");\n");
			b.append("}\n");
			b.append("catch (final InterruptedException ex)\n");
			b.append("{\n");
			b.append("Thread.currentThread().interrupt();\n");

			if (generator != null)
				generator.addErrorLog(b, "Data exchange thread has been interrupted!", "ex");

			b.append("}\n\n");
		}

		return b.toString();
	}

	/**
	 * @param addReturnType
	 * @return the method's signature
	 */
	public String getMethodSignature(boolean addReturnType) {
		final var identifier = new StringBuilder();
		boolean firstParam = true;

		if (addReturnType) {
			if (method.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE) {
				identifier.append(method.getReturnTypeModifier().toString() + "<");
				identifier.append(method.getReturnType().getName() + ">");
			}
			else
				identifier.append(method.getReturnType().getName());

			identifier.append(" ");
		}

		identifier.append(method.getName() + "(");

		for (final MethodParameter param : method.getMethodParameters()) {
			if (firstParam)
				firstParam = false;
			else
				identifier.append(", ");

			if (param.getModifier() != JavaTypeModifierEnumeration.NONE) {
				identifier.append(param.getModifier().toString() + "<");
				identifier.append(param.getType().getName() + ">");
			}
			else
				identifier.append(param.getType().getName());

			identifier.append(" " + param.getName());
		}

		identifier.append(")");

		return identifier.toString();
	}

	/**
	 * @param element
	 * @return the generated content
	 */
	private String addBatchIndexFileNameElement(String element) {
		final var b = new StringBuilder();

		element = element.replace(FILE_NAME_BATCHINDEX_ELEMENT, "").trim();
		element = REPLACEMENT_PATTERN.matcher(element).replaceFirst("").trim();

		boolean save = true;
		var format = "";

		// Extract the format settings
		for (final Character c : element.toCharArray()) {
			if (c.equals(FILE_NAME_FUNCTION_END))
				save = false;

			if (save)
				format += c;
		}

		if (format.isEmpty())
			b.append(FILE_NAME + " += " + BATCH_INDEX + ";\n");
		else
			b.append(FILE_NAME + " += new java.text.DecimalFormat(\"" + format + "\").format(" + BATCH_INDEX + ");\n");

		return b.toString();
	}

	/**
	 * @param element
	 * @return the generated content
	 */
	private String addDateFileNameElement(String element) {
		final var b = new StringBuilder();

		element = element.replace(FILE_NAME_DATE_ELEMENT, "").trim();
		element = REPLACEMENT_PATTERN.matcher(element).replaceFirst("").trim();

		boolean save = false;
		var format = "";

		// Extract the format settings
		for (final Character c : element.toCharArray()) {
			if (c.equals(FILE_NAME_FUNCTION_END))
				save = false;

			if (save)
				format += c;

			if (c.equals(FILE_NAME_PARAM_DELIMITER))
				save = true;
		}

		format = format.trim();

		// We won't generate an element if the format is missing!
		if (format.isEmpty())
			return b.toString();

		if (element.startsWith(CURRENT_DATE))
			b.append(FILE_NAME + " += new java.text.SimpleDateFormat(\"" + format + "\").format(new Date());\n");
		else if (method.getMethodType() == DataExchangeMethodTypeEnumeration.EXPORT && method.isProcessSingleObject()) {
			var attributeName = "";

			for (final Character c : element.toCharArray()) {
				if (c.equals(FILE_NAME_FUNCTION_END))
					save = false;

				if (save)
					attributeName += c;

				if (c.equals(FILE_NAME_FUNCTION_START))
					save = true;
			}

			for (final DomainAttribute attr : exchangeBean.getDomainObject().getAllAttributes())
				if (attr.getName().equals(attributeName.trim())) {
					b.append(FILE_NAME + " += new java.text.SimpleDateFormat(\"" + format + "\").format(");
					b.append(DEFAULT_DOMAIN_OBJECT_NAME + "." + attr.getGetterName() + ");\n");
				}
		}

		return b.toString();
	}

	/**
	 * @param element
	 * @return the generated content
	 */
	private String addAttributeFileNameElement(String element) {
		final var b = new StringBuilder();

		element = element.replace(FILE_NAME_ATTR_ELEMENT, "").trim();
		element = REPLACEMENT_PATTERN.matcher(element).replaceFirst("").trim();

		boolean save = false;
		var format = "";

		// Extract the format settings
		for (final Character c : element.toCharArray()) {
			if (c.equals(FILE_NAME_FUNCTION_END))
				save = false;

			if (save)
				format += c;

			if (c.equals(FILE_NAME_PARAM_DELIMITER))
				save = true;
		}

		format = format.trim();
		var attributeName = "";

		for (final Character c : element.toCharArray()) {
			if (c.equals(FILE_NAME_FUNCTION_END) || c.equals(FILE_NAME_PARAM_DELIMITER))
				break;

			attributeName += c;
		}

		for (final DomainAttribute attr : exchangeBean.getDomainObject().getAllAttributes())
			if (attr.getName().equals(attributeName.trim())) {
				if (format.isEmpty() || attr.getJavaType().isString() || attr.getJavaType().isUUID())
					b.append(FILE_NAME + " += " + DEFAULT_DOMAIN_OBJECT_NAME + "." + attr.getGetterName() + ";\n");
				else if (attr.getJavaType().isLong() || attr.getJavaType().isInteger()) {
					b.append(FILE_NAME + " += new java.text.DecimalFormat(\"" + format + "\").format(");
					b.append(DEFAULT_DOMAIN_OBJECT_NAME + "." + attr.getGetterName() + ");\n");
				}
				else if (attr.getJavaType().isDateOrCalendar()) {
					b.append(FILE_NAME + " += new java.text.SimpleDateFormat(\"" + format + "\").format(");
					b.append(DEFAULT_DOMAIN_OBJECT_NAME + "." + attr.getGetterName() + ");\n");
				}
				else if (attr.getJavaType().isLocalDate() || attr.getJavaType().isLocalDateTime()) {
					b.append(FILE_NAME + " += " + PACK_JAVA_TIME_FORMAT + ".DateTimeFormatter.ofPattern(\"" + format + "\").withZone(");
					b.append(PACK_JAVA_TIME + ".ZoneId.systemDefault()).format(");
					b.append(DEFAULT_DOMAIN_OBJECT_NAME + "." + attr.getGetterName() + ");\n");
				}
			}

		return b.toString();
	}

	/**
	 * @return the (dynamic) file name definition
	 */
	protected String createFileName() {
		final var b = new StringBuilder();

		if (fileExchangeMode == null)
			return b.toString();

		b.append("\nString " + FILE_NAME + " = ");

		final String path = fileExchangeMode.getPath();

		// Initialize the file name with a respective delimiter if the path doesn't end with a delimiter!
		if (!path.endsWith("/") && !path.endsWith("\\")) {
			if (path.contains("/"))
				b.append("\"/\"");
			else
				b.append("\"\\\\\"");
		}
		else
			b.append("\"\"");

		b.append(";\n");

		var element = "";

		// Search for dynamic file name elements
		for (final Character c : fileExchangeMode.getFileNamePattern().toCharArray()) {
			if (c.equals(DYNAMIC_FILE_NAME_ELEMENT_START)) {
				if (!element.isEmpty())
					b.append(FILE_NAME + " += \"" + element + "\";\n");

				element = "";
			}
			else if (c.equals(DYNAMIC_FILE_NAME_ELEMENT_END)) {
				element = element.trim();

				if (element.startsWith(FILE_NAME_DATE_ELEMENT))
					b.append(addDateFileNameElement(element));

				// Dynamic file name elements based on attributes or batch indexes only make sense in case of export operations
				if (method.getMethodType() == DataExchangeMethodTypeEnumeration.EXPORT) {
					if (element.startsWith(FILE_NAME_ATTR_ELEMENT) && method.isProcessSingleObject())
						b.append(addAttributeFileNameElement(element));

					if (element.startsWith(FILE_NAME_BATCHINDEX_ELEMENT) && !method.isProcessSingleObject())
						b.append(addBatchIndexFileNameElement(element));
				}

				element = "";
			}
			else
				element += c;
		}

		// Add the last element
		if (!element.isEmpty())
			b.append(FILE_NAME + " += \"" + element + "\";\n");

		b.append("\n");

		return b.toString();
	}

	/**
	 * Add method annotations
	 * @return the generated content
	 */
	protected String addAnnotations() {
		final var b = new StringBuilder();
		boolean addSecurityAnnotation = true;

		if (generator != null)
			b.append(generator.getAnnotationForGeneratedElement());

		if (project.isJavaSEApplication())
			return b.toString();

		if (project.isSpringBootApplication() && method.getMethodInvocation() != null)
			addSecurityAnnotation = false;

		if (addSecurityAnnotation) {
			if (method.getPermissionMode() == PermissionModeEnumeration.DENY_ALL)
				b.append("@DenyAll\n");
			else if (method.getPermissionMode() == PermissionModeEnumeration.PERMIT_ALL)
				b.append("@PermitAll\n");
			else if (!method.getRoles().isEmpty()) {
				boolean isFirstRole = true;

				for (final Role role : method.getRoles())
					if (isFirstRole) {
						isFirstRole = false;
						b.append("@RolesAllowed({\"" + role.getName() + "\"");
					}
					else
						b.append(", \"" + role.getName() + "\"");

				b.append("})\n");
			}
			else
				b.append("@DenyAll\n");
		}

		if (method.getMethodInvocation() != null) {
			if (project.isJakartaEEApplication()) {
				if (method.getMethodInvocation() instanceof AsynchronousInvocation)
					b.append("@Asynchronous\n");
				else if (method.getMethodInvocation() instanceof final ScheduledInvocation invocation) {
					boolean attributeSet = false;

					b.append("@Schedule(");

					if (invocation.getSecond() != null && !invocation.getSecond().isEmpty()) {
						b.append("second=\"" + invocation.getSecond() + "\"");

						attributeSet = true;
					}

					if (invocation.getMinute() != null && !invocation.getMinute().isEmpty()) {
						if (attributeSet)
							b.append(", ");
						else
							attributeSet = true;

						b.append("minute=\"" + invocation.getMinute() + "\"");
					}

					if (invocation.getHour() != null && !invocation.getHour().isEmpty()) {
						if (attributeSet)
							b.append(", ");
						else
							attributeSet = true;

						b.append("hour=\"" + invocation.getHour() + "\"");
					}

					if (invocation.getDayOfWeek() != null && !invocation.getDayOfWeek().isEmpty()) {
						if (attributeSet)
							b.append(", ");
						else
							attributeSet = true;

						b.append("dayOfWeek=\"" + invocation.getDayOfWeek() + "\"");
					}

					if (invocation.getDayOfMonth() != null && !invocation.getDayOfMonth().isEmpty()) {
						if (attributeSet)
							b.append(", ");
						else
							attributeSet = true;

						b.append("dayOfMonth=\"" + invocation.getDayOfMonth() + "\"");
					}

					if (invocation.getMonth() != null && !invocation.getMonth().isEmpty()) {
						if (attributeSet)
							b.append(", ");
						else
							attributeSet = true;

						b.append("month=\"" + invocation.getMonth() + "\"");
					}

					if (invocation.getYear() != null && !invocation.getYear().isEmpty()) {
						if (attributeSet)
							b.append(", ");

						b.append("year=\"" + invocation.getYear() + "\"");
					}

					b.append(")\n");
				}
			}
			else if (method.getMethodInvocation() instanceof AsynchronousInvocation)
				b.append("@Async\n");
			else if (method.getMethodInvocation() instanceof final ScheduledInvocation invocation) {
				b.append("@Scheduled(cron=\"");

				if (invocation.getSecond() != null && !invocation.getSecond().isEmpty())
					b.append(invocation.getSecond());
				else
					b.append("*");

				b.append(" ");

				if (invocation.getMinute() != null && !invocation.getMinute().isEmpty())
					b.append(invocation.getMinute());
				else
					b.append("*");

				b.append(" ");

				if (invocation.getHour() != null && !invocation.getHour().isEmpty())
					b.append(invocation.getHour());
				else
					b.append("*");

				b.append(" ");

				if (invocation.getDayOfMonth() != null && !invocation.getDayOfMonth().isEmpty())
					b.append(invocation.getDayOfMonth());
				else
					b.append("*");

				b.append(" ");

				if (invocation.getMonth() != null && !invocation.getMonth().isEmpty())
					b.append(invocation.getMonth());
				else
					b.append("*");

				b.append(" ");

				if (invocation.getDayOfWeek() != null && !invocation.getDayOfWeek().isEmpty())
					b.append(invocation.getDayOfWeek());
				else
					b.append("*");

				b.append("\")\n");
			}
		}

		return b.toString();
	}

}
