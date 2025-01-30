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
package net.codecadenza.eclipse.generator.domain;

import static net.codecadenza.eclipse.shared.Constants.LISTENER_SUFFIX;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;

/**
 * <p>
 * Generator for domain object callback listeners
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CallbackListenerGenerator extends AbstractJavaSourceGenerator {
	private final DomainObject domainObject;

	/**
	 * Constructor
	 * @param domainObject
	 */
	public CallbackListenerGenerator(DomainObject domainObject) {
		super(domainObject.getListenerSourceFile());

		this.domainObject = domainObject;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage("jakarta.persistence");
		importPackage(domainObject.getNamespace().toString());

		for (final DomainAttribute attr : domainObject.getAttributes())
			if ((attr.isInsertable() && attr.isSetDateOnPersist()) || (attr.isUpdatable() && attr.isSetDateOnUpdate()))
				importPackage(attr.getJavaType().getNamespace().toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public class " + domainObject.getName() + LISTENER_SUFFIX);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final var doPrePersistIdentifier = "void doPrePersist(" + domainObject.getName() + " object)";
		boolean fieldAdded = false;

		var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Method to be executed before persisting a " + domainObject.getLabel() + " object\n");
		b.append(" * @param object the " + domainObject.getLabel() + " object\n");
		b.append(" */\n");
		b.append("@PrePersist\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + doPrePersistIdentifier + "\n");
		b.append("{\n");

		for (final DomainAttribute attr : domainObject.getAttributes()) {
			if (!attr.isInsertable() || !attr.isSetDateOnPersist())
				continue;

			final String methodName = attr.getSetterName();

			fieldAdded = true;

			if (attr.getJavaType().isDateOrCalendar())
				b.append("object." + methodName + "(new " + attr.getJavaType().getName() + "());\n");
			else
				b.append("object." + methodName + "(" + attr.getJavaType().getName() + ".now());\n");
		}

		if (!fieldAdded)
			b.append("// No implementation required!\n");

		b.append("}\n\n");

		addMethod(doPrePersistIdentifier, b.toString());

		final var doPreUpdateIdentifier = "void doPreUpdate(" + domainObject.getName() + " object)";
		fieldAdded = false;

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Method to be executed before merging a " + domainObject.getLabel() + " object\n");
		b.append(" * @param object the " + domainObject.getLabel() + " object\n");
		b.append(" */\n");
		b.append("@PreUpdate\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + doPreUpdateIdentifier + "\n");
		b.append("{\n");

		for (final DomainAttribute attr : domainObject.getAttributes()) {
			if (!attr.isUpdatable() || !attr.isSetDateOnUpdate())
				continue;

			final String methodName = attr.getSetterName();

			fieldAdded = true;

			if (attr.getJavaType().isDateOrCalendar())
				b.append("object." + methodName + "(new " + attr.getJavaType().getName() + "());\n");
			else
				b.append("object." + methodName + "(" + attr.getJavaType().getName() + ".now());\n");
		}

		if (!fieldAdded)
			b.append("// No implementation required!\n");

		b.append("}\n\n");

		addMethod(doPreUpdateIdentifier, b.toString());
	}

}
