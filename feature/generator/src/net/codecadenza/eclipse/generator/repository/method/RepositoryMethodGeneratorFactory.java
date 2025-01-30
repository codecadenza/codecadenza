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
package net.codecadenza.eclipse.generator.repository.method;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.repository.method.imp.AddToAssociatonRepositoryMethodGenerator;
import net.codecadenza.eclipse.generator.repository.method.imp.ChangeParentRepositoryMethodGenerator;
import net.codecadenza.eclipse.generator.repository.method.imp.CopyRepositoryMethodGenerator;
import net.codecadenza.eclipse.generator.repository.method.imp.ExistsByUniqueKeyRepositoryMethodGenerator;
import net.codecadenza.eclipse.generator.repository.method.imp.FindByObjectRepositoryMethodGenerator;
import net.codecadenza.eclipse.generator.repository.method.imp.FindByUniqueKeyRepositoryMethodGenerator;
import net.codecadenza.eclipse.generator.repository.method.imp.GetAssociationRepositoryMethodGenerator;
import net.codecadenza.eclipse.generator.repository.method.imp.MergeRepositoryMethodGenerator;
import net.codecadenza.eclipse.generator.repository.method.imp.PersistRepositoryMethodGenerator;
import net.codecadenza.eclipse.generator.repository.method.imp.RemoveFromAssociationRepositoryMethodGenerator;
import net.codecadenza.eclipse.generator.repository.method.imp.SearchByUniqueKeyRepositoryMethodGenerator;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;

/**
 * <p>
 * Factory for repository method generators
 * </p>
 * <p>
 * Copyright 2020 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RepositoryMethodGeneratorFactory {
	/**
	 * Prevent instantiation
	 */
	private RepositoryMethodGeneratorFactory() {

	}

	/**
	 * @param repositoryMethod
	 * @param parentGenerator
	 * @return a repository method generator depending on the method type
	 * @throws IllegalStateException if a generator for the requested repository method type doesn't exist
	 */
	public static BasicRepositoryMethodGenerator getMethodGenerator(RepositoryMethod repositoryMethod,
			AbstractJavaSourceGenerator parentGenerator) {
		if (repositoryMethod.isGenerationOmitted())
			return new BasicRepositoryMethodGenerator(repositoryMethod);

		return switch (repositoryMethod.getMethodType()) {
			case CHANGE_PARENT -> new ChangeParentRepositoryMethodGenerator(repositoryMethod);
			case COPY -> new CopyRepositoryMethodGenerator(repositoryMethod, parentGenerator);
			case FIND_BY_OBJECT -> new FindByObjectRepositoryMethodGenerator(repositoryMethod);
			case GET_ASSOCIATION -> new GetAssociationRepositoryMethodGenerator(repositoryMethod);
			case ADD_TO_ASSOCIATION -> new AddToAssociatonRepositoryMethodGenerator(repositoryMethod);
			case REMOVE_FROM_ASSOCIATION -> new RemoveFromAssociationRepositoryMethodGenerator(repositoryMethod);
			case MERGE -> new MergeRepositoryMethodGenerator(repositoryMethod);
			case PERSIST -> new PersistRepositoryMethodGenerator(repositoryMethod);
			case EXISTS_BY_UNIQUE_KEY -> new ExistsByUniqueKeyRepositoryMethodGenerator(repositoryMethod);
			case SEARCH_BY_UNIQUE_KEY -> new SearchByUniqueKeyRepositoryMethodGenerator(repositoryMethod);
			case FIND_BY_UNIQUE_KEY -> new FindByUniqueKeyRepositoryMethodGenerator(repositoryMethod);
			case EXISTS_BY_UNIQUE_KEY_WITH_ID -> new ExistsByUniqueKeyRepositoryMethodGenerator(repositoryMethod);
			default -> {
				final var msg = "A repository method generator for the method type '" + repositoryMethod.getMethodType()
						+ "' is not available!";
				throw new IllegalStateException(msg);
			}
		};
	}

}
