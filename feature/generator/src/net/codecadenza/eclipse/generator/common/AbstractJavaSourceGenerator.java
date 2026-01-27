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
package net.codecadenza.eclipse.generator.common;

import static net.codecadenza.eclipse.shared.Constants.GENERATED_ELEMENT_ANNOTATION;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import net.codecadenza.eclipse.generator.CodeCadenzaGeneratorPlugin;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.tools.ide.JavaFileParser;

/**
 * <p>
 * Abstract base class for all Java source file generators that must be able to restore manual changes
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractJavaSourceGenerator {
	private final Map<String, String> importMap = new HashMap<>();
	private final Map<String, String> declarationMap = new HashMap<>();
	private final Map<String, String> customizedMethodMap = new HashMap<>();
	private final Map<String, String> customizedSubClassMap = new HashMap<>();
	private final Map<String, String> customizedConstructorMap = new HashMap<>();
	private final List<String> injectedFields = new ArrayList<>();
	private final List<String> protectedMethods = new ArrayList<>();
	private final List<String> protectedSubClasses = new ArrayList<>();
	private JavaFile javaFile;
	private StringBuilder classBody = new StringBuilder();
	private boolean addLogger;
	private boolean protectManualChanges;

	/**
	 * Constructor
	 */
	protected AbstractJavaSourceGenerator() {
	}

	/**
	 * Constructor
	 * @param javaFile
	 */
	protected AbstractJavaSourceGenerator(JavaFile javaFile) {
		this.javaFile = javaFile;
	}

	/**
	 * @return the content of the Java source file
	 */
	protected String createContent() {
		final var fullContent = new StringBuilder();
		final var fieldContent = new StringBuilder();
		final var classContent = new StringBuilder();

		if (protectManualChanges) {
			final var parser = new JavaFileParser(javaFile);

			try {
				parser.parse();

				importMap.putAll(parser.getImportMap());
				declarationMap.putAll(parser.getDeclarationMap());
				customizedMethodMap.putAll(parser.getCustomizedMethodMap());
				customizedConstructorMap.putAll(parser.getCustomizedConstructorMap());
				protectedMethods.addAll(parser.getProtectedMethods());
				customizedSubClassMap.putAll(parser.getCustomizedSubClassMap());
				protectedSubClasses.addAll(parser.getProtectedSubClasses());
			}
			catch (final Exception e) {
				// Parsing has failed! We proceed with a "graceful degradation" strategy!
				CodeCadenzaGeneratorPlugin.getInstance().logError(e);
			}
		}

		addClassDeclaration(classContent);

		classContent.append("\n");
		classContent.append("{\n");

		addFields();

		// Add fields that have been added manually
		if (protectManualChanges)
			declarationMap.values().forEach(declaration -> fieldContent.append(declaration + "\n"));

		if (!fieldContent.isEmpty()) {
			classBody.append(fieldContent);
			classBody.append("\n");
		}

		// Add constructors that have been changed manually
		if (protectManualChanges)
			customizedConstructorMap.values().forEach(constructor -> classBody.append(constructor + "\n\n"));

		addConstructors();

		if (protectManualChanges) {
			// Add methods that have been either changed or created manually
			customizedMethodMap.values().forEach(method -> classBody.append(method + "\n\n"));

			protectedMethods.forEach(method -> classBody.append(method + "\n\n"));
		}

		addMethods();

		if (protectManualChanges) {
			// Add subclasses that have been either changed or created manually
			customizedSubClassMap.values().forEach(subClass -> classBody.append(subClass + "\n\n"));

			protectedSubClasses.forEach(subClass -> classBody.append(subClass + "\n\n"));

			// Automatically import the package that contains internal generator annotations
			if (!classBody.isEmpty())
				importClass(GENERATED_ELEMENT_ANNOTATION);
		}

		if (addLogger)
			new LoggingGenerator(protectManualChanges).addField(classContent);

		classContent.append(classBody);
		classContent.append("}");

		addImports();

		if (addLogger)
			addImports(new LoggingGenerator(protectManualChanges).getImports());

		importMap.keySet().forEach(importStatement -> fullContent.append(importStatement + "\n"));

		fullContent.append(classContent);

		return fullContent.toString();
	}

	/**
	 * @return the Java source file that should be parsed and rebuilt. This method is ignored if the file has been initialized by
	 *         the respective constructor!
	 */
	protected JavaFile initJavaFile() {
		return null;
	}

	/**
	 * Add imports to the source file
	 */
	protected void addImports() {

	}

	/**
	 * Add the class declaration of the source file
	 * @param b the parameter, where content should be added to
	 */
	@SuppressWarnings("unused")
	protected void addClassDeclaration(StringBuilder b) {

	}

	/**
	 * Add fields to the source file
	 */
	protected void addFields() {

	}

	/**
	 * Add the constructors to the source file
	 */
	protected void addConstructors() {
		addDefaultConstructorForInjection(Collections.emptyList());
		addConstructorForInjection(null, Collections.emptyList());
	}

	/**
	 * Add methods to the source file
	 */
	protected void addMethods() {

	}

	/**
	 * Create the source file in the Eclipse workspace. If the file already exists the content will be overwritten!
	 * @throws IllegalStateException if the internal field 'javaFile' has not been initialized
	 * @throws Exception if the generation has failed due to an internal error
	 */
	public void createSourceFile() throws Exception {
		if (javaFile == null)
			javaFile = initJavaFile();

		if (javaFile == null)
			throw new IllegalStateException("The field 'javaFile' has not been initialized!");

		protectManualChanges = javaFile.getProject().isProtectManualChanges();
		javaFile.setContent(createContent());

		EclipseIDEService.createJavaFile(javaFile);
	}

	/**
	 * Import a set of packages
	 * @param imports
	 */
	public void addImports(Set<String> imports) {
		imports.forEach(imp -> importMap.put(imp, null));
	}

	/**
	 * Import a package
	 * @param packageName
	 */
	public void importPackage(String packageName) {
		importMap.put("import " + packageName + ".*;", null);
	}

	/**
	 * Add a static import declaration
	 * @param importDeclaration
	 */
	public void importStatic(String importDeclaration) {
		importMap.put("import static " + importDeclaration + ".*;", null);
	}

	/**
	 * Import a dedicated class
	 * @param className
	 */
	public void importClass(String className) {
		importMap.put("import " + className + ";", null);
	}

	/**
	 * Import a dedicated static class
	 * @param packageName
	 */
	public void importStaticClass(String packageName) {
		importMap.put("import static " + packageName + ";", null);
	}

	/**
	 * @return all imports
	 */
	public Set<String> getImports() {
		return importMap.keySet().stream().collect(Collectors.toSet());
	}

	/**
	 * Add a constructor. It won't be added if a customized constructor with the same signature already exists!
	 * @param methodSignature
	 * @param content
	 */
	public void addConstructor(String methodSignature, String content) {
		if (protectManualChanges && hasCustomizedConstructor(methodSignature))
			return;

		classBody.append(content);
	}

	/**
	 * Add a subclass to this source file. It won't be added if a customized subclass with the same name already exists!
	 * @param name
	 * @param content
	 */
	public void addSubClass(String name, String content) {
		if (protectManualChanges && hasCustomizedSubClass(name))
			return;

		classBody.append(content);
	}

	/**
	 * @param name
	 * @return true if this source file already contains a customized subclass with the given name
	 */
	public boolean hasCustomizedSubClass(String name) {
		return customizedSubClassMap.containsKey(name);
	}

	/**
	 * Add a method to this source file. It won't be added if a customized method with the same signature already exists!
	 * @param methodSignature
	 * @param content
	 */
	public void addMethod(String methodSignature, String content) {
		if (protectManualChanges && hasCustomizedMethod(methodSignature))
			return;

		classBody.append(content);
	}

	/**
	 * Create a getter method based on the provided type name and field name
	 * @param typeName
	 * @param fieldName
	 * @param comment
	 */
	public void addGetter(String typeName, String fieldName, String comment) {
		addGetter(typeName, fieldName, comment, false);
	}

	/**
	 * Create a getter and a setter method based on the provided type name and field name
	 * @param typeName
	 * @param fieldName
	 * @param comment
	 */
	public void addGetterAndSetter(String typeName, String fieldName, String comment) {
		addGetter(typeName, fieldName, comment, true);
	}

	/**
	 * Create a getter and a setter method based on the provided type name and field name
	 * @param typeName
	 * @param fieldName
	 * @param comment
	 * @param addSetter flag that controls if a setter should be added
	 */
	private void addGetter(String typeName, String fieldName, String comment, boolean addSetter) {
		initFieldGenerator(typeName, fieldName, JavaFieldGenerator.VISIBILITY_PUBLIC).createGetter(comment);

		if (addSetter)
			initFieldGenerator(typeName, fieldName, JavaFieldGenerator.VISIBILITY_PUBLIC).createSetter();
	}

	/**
	 * Add a public field
	 * @param typeName
	 * @param fieldName
	 * @return the field generator
	 */
	public JavaFieldGenerator addPublicField(String typeName, String fieldName) {
		return initFieldGenerator(typeName, fieldName, JavaFieldGenerator.VISIBILITY_PUBLIC);
	}

	/**
	 * Add a protected field
	 * @param typeName
	 * @param fieldName
	 * @return the field generator
	 */
	public JavaFieldGenerator addProtectedField(String typeName, String fieldName) {
		return initFieldGenerator(typeName, fieldName, JavaFieldGenerator.VISIBILITY_PROTECTED);
	}

	/**
	 * Add a private field
	 * @param typeName
	 * @param fieldName
	 * @return the field generator
	 */
	public JavaFieldGenerator addPrivateField(String typeName, String fieldName) {
		return initFieldGenerator(typeName, fieldName, JavaFieldGenerator.VISIBILITY_PRIVATE);
	}

	/**
	 * Add a field without an access modifier
	 * @param typeName
	 * @param fieldName
	 * @return the field generator
	 */
	public JavaFieldGenerator addFieldWithoutAccessModifier(String typeName, String fieldName) {
		return initFieldGenerator(typeName, fieldName, "");
	}

	/**
	 * Add a field with package visibility and a default value
	 * @param typeName
	 * @param fieldName
	 * @param defaultValue
	 * @return the field generator
	 */
	public JavaFieldGenerator addField(String typeName, String fieldName, String defaultValue) {
		return initFieldGenerator(typeName, fieldName, null).withDefaultValue(defaultValue);
	}

	/**
	 * Add a public constant
	 * @param typeName
	 * @param fieldName
	 * @param defaultValue
	 * @return the field generator
	 */
	public JavaFieldGenerator addPublicConstant(String typeName, String fieldName, String defaultValue) {
		final JavaFieldGenerator fieldGenerator = initFieldGenerator(typeName, fieldName, JavaFieldGenerator.VISIBILITY_PUBLIC);
		fieldGenerator.asConstant(defaultValue);

		return fieldGenerator;
	}

	/**
	 * Create a protected constant
	 * @param typeName
	 * @param fieldName
	 * @param defaultValue
	 * @return the field generator
	 */
	public JavaFieldGenerator addProtectedConstant(String typeName, String fieldName, String defaultValue) {
		final JavaFieldGenerator fieldGenerator = initFieldGenerator(typeName, fieldName, JavaFieldGenerator.VISIBILITY_PROTECTED);
		fieldGenerator.asConstant(defaultValue);

		return fieldGenerator;
	}

	/**
	 * Create a private constant
	 * @param typeName
	 * @param fieldName
	 * @param defaultValue
	 * @return the field generator
	 */
	public JavaFieldGenerator addPrivateConstant(String typeName, String fieldName, String defaultValue) {
		final JavaFieldGenerator fieldGenerator = initFieldGenerator(typeName, fieldName, JavaFieldGenerator.VISIBILITY_PRIVATE);
		fieldGenerator.asConstant(defaultValue);

		return fieldGenerator;
	}

	/**
	 * @param declaration
	 * @return true if this source file already contains a field with the given declaration
	 */
	public boolean hasCustomizedField(String declaration) {
		return declarationMap.containsKey(declaration);
	}

	/**
	 * @param content
	 */
	public void addStaticInitializationBlock(String content) {
		classBody.append(content);
	}

	/**
	 * @param methodSignature
	 * @return true if this source file already contains a customized constructor with the given signature
	 */
	public boolean hasCustomizedConstructor(String methodSignature) {
		return customizedConstructorMap.containsKey(methodSignature);
	}

	/**
	 * @param methodSignature
	 * @return true if this source file already contains a customized method with the given signature
	 */
	public boolean hasCustomizedMethod(String methodSignature) {
		return customizedMethodMap.containsKey(methodSignature);
	}

	/**
	 * Add a debug log statement with an arbitrary number of parameters
	 * @param b the {@link StringBuilder} where the log statement should be added to
	 * @param message the message to be logged
	 * @param parameters a list with all parameters that should be used in the log statement
	 */
	public void addDebugLog(StringBuilder b, String message, String... parameters) {
		addLogger = true;

		LoggingGenerator.addDebugLog(b, message, parameters);
	}

	/**
	 * Add an info log statement with an arbitrary number of parameters
	 * @param b the {@link StringBuilder} where the log statement should be added to
	 * @param message the message to be logged
	 * @param parameters a list with all parameters that should be used in the log statement
	 */
	public void addInfoLog(StringBuilder b, String message, String... parameters) {
		addLogger = true;

		LoggingGenerator.addInfoLog(b, message, parameters);
	}

	/**
	 * Add a warning log statement with an arbitrary number of parameters
	 * @param b the {@link StringBuilder} where the log statement should be added to
	 * @param message the message to be logged
	 * @param exception the optional name of the object that represents the exception
	 * @param parameters a list with all parameters that should be used in the log statement
	 */
	public void addWarningLog(StringBuilder b, String message, String exception, String... parameters) {
		addLogger = true;

		LoggingGenerator.addWarningLog(b, message, exception, parameters);
	}

	/**
	 * Add an error log statement with an arbitrary number of parameters
	 * @param b the {@link StringBuilder} where the log statement should be added to
	 * @param message the message to be logged
	 * @param exception the optional name of the object that represents the exception
	 * @param parameters a list with all parameters that should be used in the log statement
	 */
	public void addErrorLog(StringBuilder b, String message, String exception, String... parameters) {
		addLogger = true;

		LoggingGenerator.addErrorLog(b, message, exception, parameters);
	}

	/**
	 * Add an error log statement with an arbitrary number of parameters
	 * @param b the {@link StringBuilder} where the log statement should be added to
	 * @param constantName the name of the constant that holds the actual error message
	 * @param exception the optional name of the object that represents the exception
	 * @param parameters a list with all parameters that should be used in the log statement
	 */
	public void addErrorLogFromConstant(StringBuilder b, String constantName, String exception, String... parameters) {
		addLogger = true;

		LoggingGenerator.addErrorLogFromConstant(b, constantName, exception, parameters);
	}

	/**
	 * Add an implementation of the template method getLogger() that defines the logger for a superclass
	 * @param superClassName the fully qualified name of the superclass
	 */
	public void addGetLoggerMethod(String superClassName) {
		addGetLoggerMethod(superClassName, "getLogger");
	}

	/**
	 * Add an implementation of the template method that defines the logger for a superclass
	 * @param superClassName the fully qualified name of the superclass
	 * @param methodName the name of the method to be implemented
	 */
	public void addGetLoggerMethod(String superClassName, String methodName) {
		addLogger = true;

		final var b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see " + superClassName + "#" + methodName + "()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("protected Logger " + methodName + "()\n");
		b.append("{\n");
		b.append("return " + LoggingGenerator.LOGGER_FIELD_NAME + ";\n");
		b.append("}\n\n");

		addMethod("Logger " + methodName + "()", b.toString());
	}

	/**
	 * @return true if manual changes should be protected
	 */
	public boolean isProtectManualChanges() {
		return protectManualChanges;
	}

	/**
	 * @return the <code>@Generated</code> annotation for a field, subclass or method if manual source code changes should be
	 *         protected when rebuilding generated code. Otherwise, an empty string will be returned!
	 */
	public String getAnnotationForGeneratedElement() {
		return protectManualChanges ? "@Generated\n" : "";
	}

	/**
	 * Add the declaration of a field that should be injected
	 * @param declaration
	 */
	public void addInjectedField(String declaration) {
		injectedFields.add(declaration);
	}

	/**
	 * Clear the internal buffer that holds the file content
	 */
	protected void clearContent() {
		classBody = new StringBuilder();
	}

	/**
	 * Add a constructor for injecting all required fields
	 * @param initializationFragment
	 * @param injectedFieldsOfSuperClass
	 */
	protected void addConstructorForInjection(String initializationFragment, List<String> injectedFieldsOfSuperClass) {
		final List<String> allInjectedFields = new ArrayList<>(injectedFieldsOfSuperClass);
		allInjectedFields.addAll(injectedFields);

		if (allInjectedFields.isEmpty())
			return;

		boolean firstParam = true;

		if (javaFile.getProject().isJakartaEEApplication())
			importPackage("jakarta.inject");

		final var signature = new StringBuilder();
		signature.append(javaFile.getClassName() + "(");

		for (final String fieldDeclaration : allInjectedFields) {
			final String typeName = getTypeName(fieldDeclaration);
			final String fieldName = getFieldName(fieldDeclaration);

			if (firstParam)
				firstParam = false;
			else
				signature.append(", ");

			signature.append(typeName + " " + fieldName);
		}

		signature.append(")");

		final var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Constructor for injecting all required beans\n");

		for (final String fieldDeclaration : allInjectedFields)
			b.append(" * @param " + getFieldName(fieldDeclaration) + "\n");

		b.append(" */\n");

		if (javaFile.getProject().isJakartaEEApplication())
			b.append("@Inject\n");

		b.append(getAnnotationForGeneratedElement());
		b.append("public ");
		b.append(signature.toString());
		b.append("\n");
		b.append("{\n");

		if (!injectedFieldsOfSuperClass.isEmpty()) {
			b.append("super(");
			b.append(injectedFieldsOfSuperClass.stream().map(this::getFieldName).reduce((field1, field2) -> field1 + ", " + field2)
					.orElseThrow());
			b.append(");\n");
		}

		if (!injectedFieldsOfSuperClass.isEmpty() && !injectedFields.isEmpty())
			b.append("\n");

		for (final String fieldDeclaration : injectedFields) {
			final String fieldName = getFieldName(fieldDeclaration);

			b.append("this." + fieldName + " = " + fieldName + ";\n");
		}

		if (initializationFragment != null && !initializationFragment.isEmpty()) {
			b.append("\n");
			b.append(initializationFragment);
		}

		b.append("}\n\n");

		addConstructor(signature.toString(), b.toString());
	}

	/**
	 * Add a default constructor for a bean in a Jakarta EE application that uses injection
	 * @param injectedFieldsOfSuperClass
	 */
	protected void addDefaultConstructorForInjection(List<String> injectedFieldsOfSuperClass) {
		final List<String> allInjectedFields = new ArrayList<>(injectedFieldsOfSuperClass);
		allInjectedFields.addAll(injectedFields);

		if (allInjectedFields.isEmpty() || !javaFile.getProject().isJakartaEEApplication())
			return;

		final var b = new StringBuilder();
		final String signature = javaFile.getClassName() + "()";

		b.append("/**\n");
		b.append(" * Default constructor\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public ");
		b.append(signature);
		b.append("\n");
		b.append("{\n");

		if (!injectedFieldsOfSuperClass.isEmpty()) {
			b.append("super(");
			b.append(
					injectedFieldsOfSuperClass.stream().map(_ -> "null").reduce((field1, field2) -> field1 + ", " + field2).orElseThrow());
			b.append(");\n");
		}

		if (!injectedFieldsOfSuperClass.isEmpty() && !injectedFields.isEmpty())
			b.append("\n");

		for (final String fieldDeclaration : injectedFields)
			b.append("this." + getFieldName(fieldDeclaration) + " = null;\n");

		b.append("}\n\n");

		addConstructor(signature, b.toString());
	}

	/**
	 * Add a setter method to inject the given resource
	 * @param typeName
	 * @param fieldName
	 */
	protected void addResourceSetterMethod(String typeName, String fieldName) {
		addResourceSetterMethod(typeName, fieldName, null);
	}

	/**
	 * Add a setter method to inject the given resource
	 * @param typeName
	 * @param fieldName
	 * @param resourceName
	 */
	protected void addResourceSetterMethod(String typeName, String fieldName, String resourceName) {
		final String resourceDeclaration = typeName + " " + fieldName;
		final var methodName = "set" + fieldName.substring(0, 1).toUpperCase() + fieldName.substring(1);
		final var methodSignature = "void " + methodName + "(" + resourceDeclaration + ")";
		final var b = new StringBuilder();

		importPackage("jakarta.annotation");

		b.append("/**\n");
		b.append(" * Inject the {@link " + typeName + "} resource\n");
		b.append(" * @param " + fieldName + "\n");
		b.append(" */\n");
		b.append("@Resource");

		if (resourceName != null && !resourceName.isEmpty())
			b.append("(name=\"" + resourceName + "\")");

		b.append("\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("this." + fieldName + " = " + fieldName + ";\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

	/**
	 * Initialize a field generator
	 * @param typeName
	 * @param fieldName
	 * @param visibility
	 * @return the field generator
	 */
	private JavaFieldGenerator initFieldGenerator(String typeName, String fieldName, String visibility) {
		return new JavaFieldGenerator(typeName, fieldName, visibility, classBody, this);
	}

	/**
	 * @param declaration the declaration of a Java variable (e.g. String test)
	 * @return the type name from the provided declaration string
	 * @throws IllegalArgumentException if the declaration doesn't contain one whitespace character at least
	 */
	private String getTypeName(String declaration) {
		if (!declaration.trim().contains(" "))
			throw new IllegalArgumentException("The string '" + declaration + "' isn't a valid declaration!");

		return declaration.trim().substring(0, declaration.trim().lastIndexOf(" "));
	}

	/**
	 * @param declaration the declaration of a Java variable (e.g. String test)
	 * @return the field name from the provided declaration string
	 * @throws IllegalArgumentException if the declaration doesn't contain one whitespace character at least
	 */
	private String getFieldName(String declaration) {
		if (!declaration.trim().contains(" "))
			throw new IllegalArgumentException("The string '" + declaration + "' isn't a valid declaration!");

		return declaration.trim().substring(declaration.trim().lastIndexOf(" ") + 1);
	}

}
