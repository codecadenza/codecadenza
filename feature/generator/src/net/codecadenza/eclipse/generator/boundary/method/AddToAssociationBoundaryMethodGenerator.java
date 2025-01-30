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
package net.codecadenza.eclipse.generator.boundary.method;

import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_FIND_BY_ID;

import java.util.HashSet;
import java.util.Set;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.common.IMethodGeneratorUtility;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.java.MethodParameter;

/**
 * <p>
 * Generator for boundary methods that add an item to a list
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AddToAssociationBoundaryMethodGenerator extends BasicBoundaryMethodGenerator {
	/**
	 * Constructor
	 * @param method
	 * @param utility
	 * @param parentGenerator
	 */
	public AddToAssociationBoundaryMethodGenerator(BoundaryMethod method, IMethodGeneratorUtility utility,
			AbstractJavaSourceGenerator parentGenerator) {
		super(method, utility, parentGenerator);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#createComment()
	 */
	@Override
	public String createComment() {
		final var b = new StringBuilder();
		b.append(method.generateBeginOfJavadocComment(true));
		b.append(" * @throws DuplicateCollectionEntryException if reference already exists in this collection!\n");
		b.append(" */\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#createMethodBody()
	 */
	@Override
	public String createMethodBody() {
		final var b = new StringBuilder();
		boolean isFirstParam = true;
		MethodParameter mtmBoundaryParam = null;
		MethodParameter firstBoundaryParam = null;

		for (final MethodParameter p : method.getMethodParameters()) {
			if (!isFirstParam) {
				mtmBoundaryParam = p;
				break;
			}

			isFirstParam = false;
			firstBoundaryParam = p;
		}

		if (firstBoundaryParam == null) {
			final var msg = "The method '" + method.getName()
					+ "' could not be created as no parameter exists and two parameters are expected!";

			throw new IllegalStateException(msg);
		}

		if (mtmBoundaryParam == null)
			throw new IllegalStateException(
					"The method '" + method.getName() + "' could not be created as the expected second parameter is missing!");

		isFirstParam = true;

		for (final MethodParameter p : method.getServiceMethod().getMethodParameters())
			if (isFirstParam)
				isFirstParam = false;
			else {
				b.append("final " + p.getType().getName() + " object = ");
				b.append(getRepositoryName() + REPO_METHOD_NAME_FIND_BY_ID + "(");
				b.append(p.getType().getName() + ".class, " + mtmBoundaryParam.getName() + ")");
				b.append(";\n\n");
			}

		b.append(getRepositoryName() + method.getServiceMethod().getName() + "(" + firstBoundaryParam.getName() + ", object);\n");

		if (addTransactionManagement)
			b.append("\ntr.commit();\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final Set<String> imports = new HashSet<>(super.getImports());
		boolean isFirstParam = true;

		imports.add("import net.codecadenza.runtime.repository.*;");

		for (final MethodParameter p : method.getServiceMethod().getMethodParameters())
			if (isFirstParam)
				isFirstParam = false;
			else
				imports.add("import " + p.getType().getNamespace().toString() + ".*;");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#getInterfaceImports()
	 */
	@Override
	public Set<String> getInterfaceImports() {
		final var imports = new HashSet<>(super.getInterfaceImports());
		imports.add("import net.codecadenza.runtime.repository.*;");

		return imports;
	}

}
