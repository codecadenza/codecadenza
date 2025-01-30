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
package net.codecadenza.eclipse.generator.boundary;

import net.codecadenza.eclipse.generator.boundary.method.BoundaryMethodGeneratorFactory;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for boundary interfaces
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class BoundaryInterfaceGenerator extends AbstractJavaSourceGenerator {
	private final BoundaryBean boundaryBean;

	/**
	 * Constructor
	 * @param boundaryBean
	 */
	public BoundaryInterfaceGenerator(BoundaryBean boundaryBean) {
		super(boundaryBean.getInterfaceSourceFile());

		this.boundaryBean = boundaryBean;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public interface " + boundaryBean.getInterfaceName());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		final Project project = boundaryBean.getNamespace().getProject();
		final BoundaryBean logOnBoundary = project.getLogOnBoundary();

		// Check if this boundary bean holds the log-on method and add role names as constants
		if (logOnBoundary != null && logOnBoundary.equals(boundaryBean))
			project.getRoles().forEach(role -> addFieldWithoutAccessModifier(JavaType.STRING, "ROLE_" + role.getName().toUpperCase())
					.withDefaultValue("\"" + role.getName().toUpperCase() + "\"").create());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		boundaryBean.getBoundaryMethods().forEach(method -> {
			final var b = new StringBuilder();

			addImports(BoundaryMethodGeneratorFactory.getMethodGenerator(method, this).getInterfaceImports());

			// Add the comment
			b.append(BoundaryMethodGeneratorFactory.getMethodGenerator(method, this).createComment());

			// Add the declaration
			b.append(getAnnotationForGeneratedElement());
			b.append(BoundaryMethodGeneratorFactory.getMethodGenerator(method, this).getMethodSignature(false));
			b.append(";\n\n");

			addMethod(BoundaryMethodGeneratorFactory.getMethodGenerator(method, this).getMethodSignature(false), b.toString());
		});
	}

}
