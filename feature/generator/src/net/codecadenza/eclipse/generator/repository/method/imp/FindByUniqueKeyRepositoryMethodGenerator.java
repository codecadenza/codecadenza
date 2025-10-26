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
package net.codecadenza.eclipse.generator.repository.method.imp;

import java.util.Set;
import net.codecadenza.eclipse.generator.repository.method.BasicRepositoryMethodGenerator;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodParameter;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;

/**
 * <p>
 * Generator for repository methods of type {@link RepositoryMethodTypeEnumeration#FIND_BY_UNIQUE_KEY}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FindByUniqueKeyRepositoryMethodGenerator extends BasicRepositoryMethodGenerator {
	/**
	 * Constructor
	 * @param method
	 */
	public FindByUniqueKeyRepositoryMethodGenerator(RepositoryMethod method) {
		super(method);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.repository.method.BasicRepositoryMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = super.getImports();
		imports.add("import jakarta.persistence.*;");
		imports.add("import java.util.*;");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.repository.method.BasicRepositoryMethodGenerator#createComment()
	 */
	@Override
	protected String createComment() {
		final var b = new StringBuilder();
		final var comment = "Find a persistent " + domainObjectLabel + " object by using the provided parameters";

		b.append("/**\n");
		b.append(" * " + comment + "\n");

		method.getMethodParameters().forEach(param -> b.append(" * @param " + param.getName() + "\n"));

		b.append(" * @return the " + domainObjectLabel + " object or null if it could not be found\n");
		b.append(" * @throws IllegalStateException if the query returned more than one object\n");
		b.append(" */\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.repository.method.BasicRepositoryMethodGenerator#createMethodBody()
	 */
	@Override
	public String createMethodBody() {
		final var b = new StringBuilder();
		b.append("final TypedQuery<" + domainObjectName + "> query = em.createNamedQuery(");
		b.append(domainObjectName + "." + method.getHint() + ", " + domainObjectName + ".class);\n");

		method.getMethodParameters().forEach(param -> {
			String queryParamName = param.getName();

			// Make sure to use the correct name defined in the named query!
			if (param instanceof final RepositoryMethodParameter repositoryParam && repositoryParam.getAssociation() != null)
				queryParamName = repositoryParam.getAssociation().getName();

			b.append("query.setParameter(" + addQueryParameterConstant(queryParamName) + ", " + param.getName() + ");\n");
		});

		b.append("\nfinal List<" + domainObjectName + "> resultList = query.getResultList();\n\n");
		b.append("if(resultList.size() <= 1)\n");
		b.append("return resultList.stream().findFirst().orElse(null);\n\n");
		b.append("throw new IllegalStateException(\"Non unique result!\");\n");

		return b.toString();
	}

}
