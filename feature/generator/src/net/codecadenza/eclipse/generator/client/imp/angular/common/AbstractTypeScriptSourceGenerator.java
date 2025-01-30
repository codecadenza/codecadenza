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
package net.codecadenza.eclipse.generator.client.imp.angular.common;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;

/**
 * <p>
 * Abstract base class for all TypeScript source file generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractTypeScriptSourceGenerator {
	public static final String SELECTOR_PREFIX = "cc-";

	private final Set<DTOBean> dependentDTOs = new HashSet<>();
	private final Set<JavaEnum> dependentEnums = new HashSet<>();
	private final Set<DomainObject> domainObjectServices = new HashSet<>();
	private final String commentText;
	private final Map<String, String> typeImports = new HashMap<>();
	private final Map<String, String> services = new HashMap<>();
	private final List<String> servicesOfSuperclass = new ArrayList<>();
	private final AngularContentFormatter formatter = new AngularContentFormatter();
	private WorkspaceFile sourceFile;

	/**
	 * Constructor
	 * @param sourceFile
	 * @param commentText
	 */
	protected AbstractTypeScriptSourceGenerator(WorkspaceFile sourceFile, String commentText) {
		this.sourceFile = sourceFile;
		this.commentText = commentText;
	}

	/**
	 * Constructor
	 * @param commentText
	 */
	protected AbstractTypeScriptSourceGenerator(String commentText) {
		this(null, commentText);
	}

	/**
	 * @return the source file that should be created
	 */
	public WorkspaceFile getSourceFile() {
		return null;
	}

	/**
	 * @return the content of the TypeScript source file
	 */
	protected String createContent() {
		formatter.addBlockComment(commentText);

		addTypeDeclaration(formatter);

		addFields();

		formatter.addBlankLine();
		formatter.increaseIndent();

		addConstructor(formatter);

		addMethods(formatter);

		formatter.decreaseIndent();

		formatter.addLine("}");

		addImports();

		final var fullContent = new StringBuilder();
		final var modulePaths = new HashMap<String, List<String>>();

		// Collect all types from the same module path
		typeImports.entrySet().forEach(type -> {
			if (!modulePaths.containsKey(type.getValue()))
				modulePaths.put(type.getValue(), new ArrayList<>());

			modulePaths.get(type.getValue()).add(type.getKey());
		});

		modulePaths.entrySet().forEach(modulePath -> {
			final String types = modulePath.getValue().stream().reduce((a, b) -> a + ", " + b).get();

			fullContent.append("import { " + types + " } from '" + modulePath.getKey() + "';\n");
		});

		if (!typeImports.isEmpty())
			fullContent.append("\n");

		fullContent.append(formatter.getContent());
		fullContent.append("\n");

		return fullContent.toString();
	}

	/**
	 * @return the formatter of this generator
	 */
	public AngularContentFormatter getContentFormatter() {
		return formatter;
	}

	/**
	 * Add imports to the source file
	 */
	protected void addImports() {

	}

	/**
	 * Add the type declaration the of source file
	 * @param formatter the parameter, where content should be added to
	 */
	@SuppressWarnings("unused")
	protected void addTypeDeclaration(AngularContentFormatter formatter) {

	}

	/**
	 * Add fields to source file
	 */
	protected void addFields() {

	}

	/**
	 * Add a constructor to the source file
	 * @param formatter
	 */
	protected void addConstructor(AngularContentFormatter formatter) {
		if (services.isEmpty())
			return;

		boolean firstService = true;

		formatter.addBlockComment("Create a new instance and inject all required services");
		formatter.addLine("constructor(");
		formatter.increaseIndent();

		for (final var entry : services.entrySet()) {
			final var override = servicesOfSuperclass.contains(entry.getValue()) ? "override " : "";
			final var injectedService = "protected " + override + entry.getValue() + " : " + entry.getKey();

			if (firstService) {
				formatter.addLine(injectedService);
				firstService = false;
			}
			else
				formatter.addLine(", " + injectedService);
		}

		formatter.decreaseIndent();
		formatter.addLine(") {");
		formatter.increaseIndent();

		final String superArguments = servicesOfSuperclass.stream().reduce((a, b) -> a + ", " + b).orElse("");

		if (!superArguments.isEmpty())
			formatter.addLine("super(" + superArguments + ");");

		addConstructorStatements(formatter);

		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.addBlankLine();
	}

	/**
	 * Add statements in the constructor after calling super()
	 * @param formatter
	 */
	@SuppressWarnings("unused")
	protected void addConstructorStatements(AngularContentFormatter formatter) {

	}

	/**
	 * Add the methods to the source file
	 * @param formatter
	 */
	@SuppressWarnings("unused")
	protected void addMethods(AngularContentFormatter formatter) {

	}

	/**
	 * Create the source file in the Eclipse workspace. If the file already exists the content will be overwritten!
	 * @throws IllegalStateException if the internal field 'sourceFile' has not been initialized
	 * @throws Exception if the generation has failed due to an internal error
	 */
	public void createSourceFile() throws Exception {
		if (sourceFile == null)
			sourceFile = getSourceFile();

		if (sourceFile == null)
			throw new IllegalStateException("The field 'sourceFile' has not been initialized!");

		sourceFile.setContent(createContent());

		EclipseIDEService.createOrUpdateFile(sourceFile);
	}

	/**
	 * Add imports
	 * @param imports
	 */
	public void addImports(Map<String, String> imports) {
		typeImports.putAll(imports);
	}

	/**
	 * Import an arbitrary number of types
	 * @param typeNames
	 * @param path
	 */
	public void importTypes(Stream<String> typeNames, String path) {
		typeImports.put(typeNames.reduce((a, b) -> a + ", " + b).orElse(""), path);
	}

	/**
	 * Import a dedicated type
	 * @param typeName
	 * @param path
	 */
	public void importType(String typeName, String path) {
		typeImports.put(typeName, path);
	}

	/**
	 * @return all imports
	 */
	public Map<String, String> getImports() {
		return typeImports;
	}

	/**
	 * Add a public field
	 * @param type
	 * @param name
	 * @return the field generator
	 */
	public TypeScriptFieldGenerator addPublicField(String type, String name) {
		return initFieldGenerator(type, name, TypeScriptFieldGenerator.VISIBILITY_PUBLIC);
	}

	/**
	 * Add a protected field
	 * @param type
	 * @param name
	 * @return the field generator
	 */
	public TypeScriptFieldGenerator addProtectedField(String type, String name) {
		return initFieldGenerator(type, name, TypeScriptFieldGenerator.VISIBILITY_PROTECTED);
	}

	/**
	 * Add a private field
	 * @param type
	 * @param name
	 * @return the field generator
	 */
	public TypeScriptFieldGenerator addPrivateField(String type, String name) {
		return initFieldGenerator(type, name, TypeScriptFieldGenerator.VISIBILITY_PRIVATE);
	}

	/**
	 * Add a field without an access modifier
	 * @param type
	 * @param name
	 * @return the field generator
	 */
	public TypeScriptFieldGenerator addField(String type, String name) {
		return initFieldGenerator(type, name, null);
	}

	/**
	 * Add a field with a default value
	 * @param type
	 * @param name
	 * @param defaultValue
	 * @return the field generator
	 */
	public TypeScriptFieldGenerator addField(String type, String name, String defaultValue) {
		return initFieldGenerator(type, name, null).withDefaultValue(defaultValue);
	}

	/**
	 * Add a public constant
	 * @param type
	 * @param name
	 * @param defaultValue
	 * @return the field generator
	 */
	public TypeScriptFieldGenerator addPublicConstant(String type, String name, String defaultValue) {
		final TypeScriptFieldGenerator fieldGenerator = initFieldGenerator(type, name, TypeScriptFieldGenerator.VISIBILITY_PUBLIC);
		fieldGenerator.asConstant(defaultValue);

		return fieldGenerator;
	}

	/**
	 * Create a protected constant
	 * @param type
	 * @param name
	 * @param defaultValue
	 * @return the field generator
	 */
	public TypeScriptFieldGenerator addProtectedConstant(String type, String name, String defaultValue) {
		final TypeScriptFieldGenerator fieldGenerator = initFieldGenerator(type, name, TypeScriptFieldGenerator.VISIBILITY_PROTECTED);
		fieldGenerator.asConstant(defaultValue);

		return fieldGenerator;
	}

	/**
	 * Create a private constant
	 * @param type
	 * @param name
	 * @param defaultValue
	 * @return the field generator
	 */
	public TypeScriptFieldGenerator addPrivateConstant(String type, String name, String defaultValue) {
		final TypeScriptFieldGenerator fieldGenerator = initFieldGenerator(type, name, TypeScriptFieldGenerator.VISIBILITY_PRIVATE);
		fieldGenerator.asConstant(defaultValue);

		return fieldGenerator;
	}

	/**
	 * Add an injected service that is required by the superclass
	 * @param type
	 * @param name
	 * @param path
	 */
	public void addServiceOfSuperclass(String type, String name, String path) {
		addService(type, name, path);

		servicesOfSuperclass.add(name);
	}

	/**
	 * Add an injected service
	 * @param type
	 * @param name
	 * @param path
	 */
	public void addService(String type, String name, String path) {
		if (path != null && !path.isEmpty())
			importType(type, path);

		services.put(type, name);
	}

	/**
	 * Add the corresponding service for the given data transfer object
	 * @param dto
	 */
	public void addService(DTOBean dto) {
		if (dto == null)
			return;

		importType(dto.getName(), "../../domain/" + dto.getName().toLowerCase() + ".interface");

		addDependentDTO(dto);

		addService(dto.getDomainObject());
	}

	/**
	 * Add the corresponding service for the given domain object
	 * @param domainObject
	 */
	public void addService(DomainObject domainObject) {
		if (domainObject == null)
			return;

		if (domainObjectServices.contains(domainObject))
			return;

		domainObjectServices.add(domainObject);

		final var serviceTypeName = domainObject.getName() + "Service";
		final var serviceName = domainObject.getLowerCaseName() + "Service";

		addService(serviceTypeName, serviceName, "../../services/" + domainObject.getName().toLowerCase() + ".service");
	}

	/**
	 * Add a dependent data transfer object
	 * @param dto
	 */
	public void addDependentDTO(DTOBean dto) {
		dependentDTOs.add(dto);
	}

	/**
	 * @return a set containing all data transfer objects on which this object depends
	 */
	public Set<DTOBean> getDependentDTOs() {
		return dependentDTOs;
	}

	/**
	 * Add a dependent enum
	 * @param javaEnum
	 */
	public void addDependentEnum(JavaEnum javaEnum) {
		dependentEnums.add(javaEnum);
	}

	/**
	 * @return a set containing all enumerations on which this object depends
	 */
	public Set<JavaEnum> getDependentEnums() {
		return dependentEnums;
	}

	/**
	 * Initialize a field generator
	 * @param type
	 * @param name
	 * @param visibility
	 * @return the field generator
	 */
	private TypeScriptFieldGenerator initFieldGenerator(String type, String name, String visibility) {
		if (type == null || type.isEmpty())
			return new TypeScriptFieldGenerator(name, visibility, formatter);

		return new TypeScriptFieldGenerator(name + ": " + type, visibility, formatter);
	}

}
