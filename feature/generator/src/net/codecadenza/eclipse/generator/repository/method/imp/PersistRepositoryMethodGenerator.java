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

import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_PERSIST;

import java.util.Set;
import net.codecadenza.eclipse.generator.repository.method.BasicRepositoryMethodGenerator;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;

/**
 * <p>
 * Generator for repository methods of type {@link RepositoryMethodTypeEnumeration#PERSIST}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class PersistRepositoryMethodGenerator extends BasicRepositoryMethodGenerator {
	/**
	 * Constructor
	 * @param method
	 */
	public PersistRepositoryMethodGenerator(RepositoryMethod method) {
		super(method);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.repository.method.BasicRepositoryMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = super.getImports();
		imports.add("import net.codecadenza.runtime.repository.*;");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.repository.method.BasicRepositoryMethodGenerator#createComment()
	 */
	@Override
	protected String createComment() {
		final var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Persist the " + domainObjectLabel + " object\n");

		method.getMethodParameters().forEach(param -> b.append(" * @param " + param.getName() + "\n"));

		b.append(" * @param performChecks flag that controls if internal data integrity checks should be performed\n");
		b.append(" * @param performFlush flag that controls if the database synchronization should be performed immediately\n");
		b.append(" * @param performRefresh flag that controls if a refresh operation should be performed after persist\n");
		b.append(" * @throws UniqueConstraintViolationException if a unique constraint check has failed\n");
		b.append(" * @throws " + getValidationExceptionName() + " if the validation of the persistent attributes has failed\n");
		b.append(" * @return the persisted " + domainObjectLabel + " object\n");
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
		final String entityParam = method.getFirstParameter().getName();

		// NOTE: Calling flush and refresh seem to be the one and only solution to avoid different problems concerning the state of
		// the persisted entity:
		// 1.) When working with Glassfish it turned out that without calling the
		// flush-method the auto-generated primary key value is not set after persist.
		// The persistence provider is free to perform a flush-operation at any time if flush-mode
		// AUTO is set!
		// 2.) Calling persist just brings the persisted entity in managed state. Detached entities
		// that are used to set many-to-one or many-to-many relationships will not be managed.
		// This can cause problems afterwards when accessing further data of these detached objects. This approach isn't optimal in
		// terms of performance but necessary to avoid aforementioned mentioned problems!
		b.append(createUniqueKeyChecks());
		b.append("return " + REPO_METHOD_NAME_PERSIST + "(" + entityParam + ", performFlush, performRefresh);\n");

		return b.toString();
	}

}
