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
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;

/**
 * <p>
 * Generator for repository methods of type {@link RepositoryMethodTypeEnumeration#GET_ASSOCIATION}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class GetAssociationRepositoryMethodGenerator extends BasicRepositoryMethodGenerator {
	/**
	 * Constructor
	 * @param method
	 */
	public GetAssociationRepositoryMethodGenerator(RepositoryMethod method) {
		super(method);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.repository.method.BasicRepositoryMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = super.getImports();
		imports.add("import " + method.getReturnType().getNamespace().toString() + ".*;");
		imports.add("import jakarta.persistence.*;");

		if (method.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
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
		final var refBean = (DomainObject) method.getReturnType();
		final String comment;

		if (method.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
			comment = "Get all " + refBean.getLabelPlural() + " of this " + domainObjectLabel;
		else
			comment = "Get the " + refBean.getLabel() + " of this " + domainObjectLabel;

		b.append("/**\n");
		b.append(" * " + comment + "\n");

		method.getMethodParameters().forEach(param -> b.append(" * @param " + param.getName() + "\n"));

		if (method.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
			b.append(" * @return a list of " + refBean.getLabelPlural() + " of this " + domainObjectLabel + "\n");
		else
			b.append(" * @return the " + refBean.getLabel() + " of this " + domainObjectLabel + ", or null if it could not be found\n");

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
		final String paramName = method.getFirstParameter().getName();

		b.append("final TypedQuery<" + method.getReturnType().getName() + "> query = em.createNamedQuery(");
		b.append(domainObjectName + "." + method.getHint() + ", " + method.getReturnType().getName() + ".class);\n");
		b.append("query.setParameter(" + addQueryParameterConstant(paramName) + ", " + paramName + ");\n\n");
		b.append("return query.getResultList()");

		if (method.getReturnTypeModifier() == JavaTypeModifierEnumeration.NONE)
			b.append(".stream().findFirst().orElse(null)");

		b.append(";\n");

		return b.toString();
	}

}
