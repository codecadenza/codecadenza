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

import static net.codecadenza.eclipse.shared.Constants.PARAM_LOGGED_ON_USER;
import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_FIND_BY_ID;

import java.util.HashSet;
import java.util.Set;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.common.IMethodGeneratorUtility;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;

/**
 * <p>
 * Generator for boundary methods that create a copy of a given object
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CopyBoundaryMethodGenerator extends BasicBoundaryMethodGenerator {
	private final RepositoryMethod repositoryMethod;
	private final MethodParameter param;

	/**
	 * Constructor
	 * @param method
	 * @param utility
	 * @param parentGenerator
	 */
	public CopyBoundaryMethodGenerator(BoundaryMethod method, IMethodGeneratorUtility utility,
			AbstractJavaSourceGenerator parentGenerator) {
		super(method, utility, parentGenerator);

		this.repositoryMethod = (RepositoryMethod) method.getServiceMethod();
		this.param = method.getFirstParameter();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#createComment()
	 */
	@Override
	public String createComment() {
		final var b = new StringBuilder();
		b.append(method.generateBeginOfJavadocComment());
		b.append(" * @param " + param.getName() + "\n");

		if (repositoryMethod.addUserParam())
			b.append(" * @param " + PARAM_LOGGED_ON_USER + "\n");

		if (method.addUniqueCheck())
			b.append(" * @throws UniqueConstraintViolationException if a unique constraint check has failed\n");

		if (!validationExceptionComment.isEmpty())
			b.append(validationExceptionComment);

		b.append(" * @return the id of the new object\n");
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
		final MethodParameter repositoryParam = repositoryMethod.getFirstParameter();
		final var targetDomainObject = (DomainObject) repositoryParam.getType();

		if (targetDomainObject == null)
			throw new IllegalStateException(
					"The target domain object for creating the boundary method '" + method.getName() + "' has not been found!");

		b.append("final " + targetDomainObject.getName() + " sourceObject = ");
		b.append(getRepositoryName() + REPO_METHOD_NAME_FIND_BY_ID + "(" + param.getName() + ");\n");
		b.append("final " + targetDomainObject.getName() + " targetObject = ");
		b.append(getRepositoryName() + method.getServiceMethod().getName() + "(sourceObject, null");

		if (repositoryMethod.addUserParam())
			b.append(", " + PARAM_LOGGED_ON_USER);

		b.append(");\n\n");

		if (addTransactionManagement)
			b.append("tr.commit();\n\n");

		b.append("return targetObject." + targetDomainObject.getPKAttribute().getGetterName() + ";\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = new HashSet<>(super.getImports());

		if (!validationExceptionImport.isEmpty())
			imports.add(validationExceptionImport);

		if (method.addUniqueCheck())
			imports.add("import net.codecadenza.runtime.repository.*;");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#getInterfaceImports()
	 */
	@Override
	public Set<String> getInterfaceImports() {
		final var imports = new HashSet<>(super.getInterfaceImports());

		if (!validationExceptionImport.isEmpty())
			imports.add(validationExceptionImport);

		if (method.addUniqueCheck())
			imports.add("import net.codecadenza.runtime.repository.*;");

		return imports;
	}

}
