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

import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.util.JavaBeanHelper;

/**
 * <p>
 * Generator for adding fields to a Java source file
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JavaFieldGenerator {
	public static final String VISIBILITY_PUBLIC = "public";
	public static final String VISIBILITY_PROTECTED = "protected";
	public static final String VISIBILITY_PRIVATE = "private";

	private final String typeName;
	private final String fieldName;
	private final String visibility;
	private final StringBuilder output;
	private final StringBuilder annotations = new StringBuilder();
	private final AbstractJavaSourceGenerator generator;
	private final boolean protectManualChanges;
	private String defaultValue;
	private boolean finalModifier;
	private boolean staticModifier;
	private boolean volatileModifier;
	private boolean transientModifier;
	private boolean resource;

	/**
	 * Constructor
	 * @param typeName
	 * @param fieldName
	 * @param visibility
	 * @param output
	 * @param generator
	 */
	public JavaFieldGenerator(String typeName, String fieldName, String visibility, StringBuilder output,
			AbstractJavaSourceGenerator generator) {
		this.typeName = typeName;
		this.fieldName = fieldName;
		this.visibility = visibility;
		this.output = output;
		this.generator = generator;
		this.protectManualChanges = generator.isProtectManualChanges();
	}

	/**
	 * Constructor
	 * @param typeName
	 * @param fieldName
	 * @param visibility
	 * @param output
	 * @param protectManualChanges
	 */
	public JavaFieldGenerator(String typeName, String fieldName, String visibility, StringBuilder output,
			boolean protectManualChanges) {
		this.typeName = typeName;
		this.fieldName = fieldName;
		this.visibility = visibility;
		this.output = output;
		this.generator = null;
		this.protectManualChanges = protectManualChanges;
	}

	/**
	 * Define a default value for this field
	 * @param defaultValue
	 * @return the field generator
	 */
	public JavaFieldGenerator withDefaultValue(String defaultValue) {
		this.defaultValue = defaultValue;

		return this;
	}

	/**
	 * Add the final modifier to this field
	 * @return the field generator
	 */
	public JavaFieldGenerator withFinalModifier() {
		finalModifier = true;

		return this;
	}

	/**
	 * Add the static modifier to this field
	 * @return the field generator
	 */
	public JavaFieldGenerator withStaticModifier() {
		staticModifier = true;

		return this;
	}

	/**
	 * Add the volatile modifier to this field
	 * @return the field generator
	 */
	public JavaFieldGenerator withVolatileModifier() {
		volatileModifier = true;

		return this;
	}

	/**
	 * Add the transient modifier to this field
	 * @return the field generator
	 */
	public JavaFieldGenerator withTransientModifier() {
		transientModifier = true;

		return this;
	}

	/**
	 * Add annotations
	 * @param annotations
	 * @return the field generator
	 */
	public JavaFieldGenerator withAnnotations(String annotations) {
		this.annotations.append(annotations);

		return this;
	}

	/**
	 * Define this field as a constant. This means that a final and a static modifier will be added!
	 * @param defaultValue
	 * @return the field generator
	 */
	public JavaFieldGenerator asConstant(String defaultValue) {
		this.defaultValue = defaultValue;

		return withStaticModifier().withFinalModifier();
	}

	/**
	 * Inject this field by using '@Inject'
	 * @return the field generator
	 */
	public JavaFieldGenerator inject() {
		final var declaration = typeName + " " + fieldName;

		if (generator != null)
			generator.addInjectedField(declaration);
		else
			throw new IllegalStateException("The field " + declaration + " cannot be injected!");

		return this.withFinalModifier();
	}

	/**
	 * Inject this field by using '@Resource'
	 * @return the field generator
	 */
	public JavaFieldGenerator asResource() {
		resource = true;

		return this;
	}

	/**
	 * Set the transient modifier
	 * @param transientModifier
	 * @return the field generator
	 */
	public JavaFieldGenerator setTransientModifier(boolean transientModifier) {
		this.transientModifier = transientModifier;

		return this;
	}

	/**
	 * Create the field and add it to the source file
	 */
	public void create() {
		final var declaration = typeName + " " + fieldName;

		if (generator != null && generator.hasCustomizedField(declaration))
			return;

		if (!annotations.toString().isEmpty())
			output.append(annotations);

		if (protectManualChanges)
			output.append("@Generated\n");

		if (visibility != null && !visibility.isEmpty())
			output.append(visibility + " ");

		if (staticModifier)
			output.append("static ");

		if (finalModifier)
			output.append("final ");

		if (transientModifier)
			output.append("transient ");

		if (volatileModifier)
			output.append("volatile ");

		if (resource) {
			generator.importPackage("jakarta.annotation");

			output.append("@Resource ");
		}

		output.append(declaration);

		if (defaultValue != null && !defaultValue.isEmpty())
			output.append(" = " + defaultValue);

		output.append(";\n");
	}

	/**
	 * Create a getter method
	 * @param comment
	 */
	public void createGetter(String comment) {
		final var getter = typeName + " " + JavaBeanHelper.getGetterName(fieldName, typeName.equals(JavaType.BOOL));

		if (generator != null && generator.hasCustomizedMethod(getter))
			return;

		output.append("/**\n");
		output.append(" * @return ");

		if (comment != null && !comment.isEmpty())
			output.append(comment);

		output.append("\n");
		output.append(" */\n");

		if (protectManualChanges)
			output.append("@Generated\n");

		output.append("public ");

		if (staticModifier)
			output.append("static ");

		output.append(getter + "\n");
		output.append("{\n");
		output.append("return " + fieldName + ";\n");
		output.append("}\n\n");
	}

	/**
	 * Create a setter method
	 */
	public void createSetter() {
		final var setter = JavaType.VOID + " " + JavaBeanHelper.getSetterName(fieldName) + "(" + typeName + " " + fieldName + ")";

		if (generator != null && generator.hasCustomizedMethod(setter))
			return;

		output.append("/**\n");
		output.append(" * @param " + fieldName + "\n");
		output.append(" */\n");

		if (protectManualChanges)
			output.append("@Generated\n");

		output.append("public ");

		if (staticModifier)
			output.append("static ");

		output.append(setter + "\n");
		output.append("{\n");
		output.append("this." + fieldName + " = " + fieldName + ";\n");
		output.append("}\n\n");
	}

}
