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

/**
 * <p>
 * Generator for adding fields to a TypeScript source file
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TypeScriptFieldGenerator {
	public static final String VISIBILITY_PUBLIC = "public";
	public static final String VISIBILITY_PROTECTED = "protected";
	public static final String VISIBILITY_PRIVATE = "private";

	private final String declaration;
	private final String visibility;
	private final AngularContentFormatter formatter;
	private String defaultValue;
	private boolean readonly;
	private boolean staticModifier;
	private boolean input;
	private boolean viewChild;
	private String viewChildName;

	/**
	 * Constructor
	 * @param declaration
	 * @param visibility
	 * @param formatter
	 */
	public TypeScriptFieldGenerator(String declaration, String visibility, AngularContentFormatter formatter) {
		this.declaration = declaration;
		this.visibility = visibility;
		this.formatter = formatter;
	}

	/**
	 * Define a default value for this field
	 * @param defaultValue
	 * @return the field generator
	 */
	public TypeScriptFieldGenerator withDefaultValue(String defaultValue) {
		this.defaultValue = defaultValue;

		return this;
	}

	/**
	 * Add the 'readonly' modifier to this field
	 * @return the field generator
	 */
	public TypeScriptFieldGenerator withReadonlyModifier() {
		readonly = true;

		return this;
	}

	/**
	 * Add the 'static' modifier to this field
	 * @return the field generator
	 */
	public TypeScriptFieldGenerator withStaticModifier() {
		staticModifier = true;

		return this;
	}

	/**
	 * Add the '@Input' modifier to this field
	 * @return the field generator
	 */
	public TypeScriptFieldGenerator withInputModifier() {
		input = true;

		return this;
	}

	/**
	 * Define this field as a constant. This means that a 'readonly' and a 'static' modifier will be added!
	 * @param defaultValue
	 * @return the field generator
	 */
	public TypeScriptFieldGenerator asConstant(String defaultValue) {
		this.defaultValue = defaultValue;

		return withStaticModifier().withReadonlyModifier();
	}

	/**
	 * Inject this field by using '@ViewChild'
	 * @param name
	 * @return the field generator
	 */
	public TypeScriptFieldGenerator asViewChild(String name) {
		viewChild = true;
		viewChildName = name;

		return this;
	}

	/**
	 * Create the field and add it to the source file
	 */
	public void create() {
		formatter.increaseIndent();

		final var b = new StringBuilder();

		if (input)
			b.append("@Input() ");
		else if (viewChild)
			b.append("@ViewChild('" + viewChildName + "', {static: false}) ");

		if (visibility != null && !visibility.isEmpty())
			b.append(visibility + " ");

		if (staticModifier)
			b.append("static ");

		if (readonly)
			b.append("readonly ");

		b.append(declaration);

		if (defaultValue != null && !defaultValue.isEmpty())
			b.append(" = " + defaultValue);

		b.append(";");

		formatter.addLine(b.toString());
		formatter.decreaseIndent();
	}

}
