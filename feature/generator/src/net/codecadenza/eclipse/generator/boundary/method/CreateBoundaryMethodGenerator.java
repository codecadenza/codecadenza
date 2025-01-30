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

import static net.codecadenza.eclipse.shared.Constants.DEFAULT_REPOSITORY;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.common.IMethodGeneratorUtility;
import net.codecadenza.eclipse.generator.dto.DTOInlineConversionGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;

/**
 * <p>
 * Generator for boundary methods that perform a create operation
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CreateBoundaryMethodGenerator extends BasicBoundaryMethodGenerator {
	protected String returnObjectName = "";

	/**
	 * Constructor
	 * @param method
	 * @param utility
	 * @param parentGenerator
	 */
	public CreateBoundaryMethodGenerator(BoundaryMethod method, IMethodGeneratorUtility utility,
			AbstractJavaSourceGenerator parentGenerator) {
		super(method, utility, parentGenerator);

		for (final MethodParameter p : method.getMethodParameters())
			if (p.getType() instanceof DTOBean) {
				this.returnObjectName = p.getName();
				break;
			}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#createComment()
	 */
	@Override
	public String createComment() {
		final var b = new StringBuilder();
		b.append(method.generateBeginOfJavadocComment(true));

		if (method.addUniqueCheck())
			b.append(" * @throws UniqueConstraintViolationException if a unique constraint check has failed\n");

		if (!validationExceptionComment.isEmpty())
			b.append(validationExceptionComment);

		if (!method.getReturnType().isVoid())
			b.append(" * @return the persisted " + domainObjectLabel + " object\n");

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
		final var entityNameSet = new HashSet<String>();
		int paramIndex = 0;
		int paramCount = 0;

		// Determine the number of parameters that represent a DTO
		for (final MethodParameter p : method.getMethodParameters())
			if (p.getType() instanceof DTOBean)
				paramCount++;

		// Iterate over all parameters that represent a DTO. We assume that every DTO represents a domain object that in turn must be
		// persisted!
		for (final MethodParameter p : method.getMethodParameters()) {
			if (!(p.getType() instanceof final DTOBean dtoBean))
				continue;

			var domainObjName = "new" + dtoBean.getDomainObject().getName();

			// Avoid name clashes if the method uses domain objects of the same type!
			if (entityNameSet.contains(domainObjName))
				domainObjName += paramIndex;

			entityNameSet.add(domainObjName);

			if (paramIndex > 0) {
				b.append("\n");
				b.append("if(" + p.getName() + " != null)\n");
				b.append("{\n");
			}
			else
				b.append("// Create new object to be persisted\n");

			final var converter = new DTOInlineConversionGenerator(method.getMethodType(), dtoBean, domainObjName, p.getName());
			boolean addNewObjectToOneToMany = false;

			b.append("var " + domainObjName + " = new " + dtoBean.getDomainObject().getName() + "();\n");
			b.append(converter.addAttributeSetters());

			if (paramIndex > 0) {
				// Set the parent for each child object!
				final DomainObject parentDO = method.getBoundaryBean().getDomainObject();
				final DomainObject paramDO = dtoBean.getDomainObject();

				for (final AbstractDomainAssociation assoc : parentDO.getAllAssociations())
					if (assoc instanceof final OneToManyAssociation oneToMany && paramDO.equals(oneToMany.getTarget())) {
						if (oneToMany.isBidirectional()) {
							final ManyToOneAssociation reverseAssoc = oneToMany.getReverseAssociation();

							b.append(domainObjName + "." + reverseAssoc.getSetterName() + "(new" + parentDO.getName() + ");\n");
						}
						else
							addNewObjectToOneToMany = true;

						break;
					}
			}

			// Invoke the persist method
			b.append(createPersistCall(domainObjName, dtoBean.getDomainObject(), addNewObjectToOneToMany, paramIndex, paramCount));

			if (addNewObjectToOneToMany) {
				// Add the new object to the unidirectional one-to-many association.
				final DomainObject parentDO = method.getBoundaryBean().getDomainObject();
				final DomainObject paramDO = dtoBean.getDomainObject();

				for (final AbstractDomainAssociation assoc : parentDO.getAllAssociations())
					if (assoc instanceof final OneToManyAssociation oneToMany && paramDO.equals(oneToMany.getTarget())
							&& !oneToMany.isBidirectional()) {
						b.append("new" + parentDO.getName() + "." + oneToMany.getGetterName() + ".add(" + domainObjName + ");\n");
						break;
					}
			}

			if (paramIndex > 0)
				b.append("}\n");

			paramIndex++;
		}

		final JavaType returnType = method.getReturnType();

		if (addTransactionManagement)
			b.append("\ntr.commit();\n");

		if (returnType instanceof DTOBean)
			b.append("\nreturn " + returnObjectName + ";\n");

		return b.toString();
	}

	/**
	 * @return a set containing all repositories that are necessary for a local invocation
	 */
	public Set<Repository> getAllRepositories() {
		final var repositories = new HashSet<Repository>();

		// There might be additional repositories that a persist method needs
		for (final MethodParameter p : method.getMethodParameters()) {
			if (!(p.getType() instanceof final DTOBean paramDTO))
				continue;

			final Collection<Repository> allRepositories = project.getAllRepositoriesOfProject();

			for (final Repository repository : allRepositories) {
				if (repository.getDomainObject().equals(paramDTO.getDomainObject())) {
					repositories.add(repository);
					break;
				}
			}
		}

		return repositories;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#getLocalServices()
	 */
	@Override
	public String getLocalServices() {
		final var b = new StringBuilder();

		// Initialize all repositories that are necessary for this method
		getAllRepositories().forEach(repository -> {
			String repositoryName = DEFAULT_REPOSITORY;

			if (!repository.getDomainObject().equals(method.getBoundaryBean().getDomainObject()))
				repositoryName = repository.getLowerCaseName();

			b.append("final var " + repositoryName + " = new " + repository.getName() + "(em);\n");
		});

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

		// There might be additional packages to be imported as a persist method can have more than one object to be persisted!
		method.getMethodParameters().forEach(param -> {
			if (param.getType() instanceof final DTOBean dto) {
				final var converter = new DTOInlineConversionGenerator(BoundaryMethodTypeEnumeration.CREATE, dto, "name",
						param.getName());
				imports.addAll(converter.getImports());
			}

			final String subNamespace = param.getType().getNamespace().getName();
			final String domainNamespace = project.getDomainNamespace().toString();
			final String dtoNamespace = project.getDTONamespace().toString();
			final String repositoryNamespace = project.getRepositoryNamespace().toString();

			// Import the DTO package
			imports.add("import " + dtoNamespace + "." + subNamespace + ".*;");

			// Import the domain package
			imports.add("import " + domainNamespace + "." + subNamespace + ".*;");

			imports.add("import " + repositoryNamespace + "." + subNamespace + ".*;");
		});

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

		method.getMethodParameters().forEach(param -> imports.add("import " + param.getType().getNamespace().toString() + ".*;"));

		return imports;
	}

	/**
	 * Create the persist method call
	 * @param domainObjectName
	 * @param domainObject
	 * @param addNewObjectToOneToMany
	 * @param paramIndex
	 * @param paramCount
	 * @return the generated content
	 */
	protected String createPersistCall(String domainObjectName, DomainObject domainObject, boolean addNewObjectToOneToMany,
			int paramIndex, int paramCount) {
		final var b = new StringBuilder();
		final JavaType returnType = method.getReturnType();
		String repositoryName = getRepositoryName();

		final RepositoryMethod repositoryMethod = project.getAllRepositoriesOfProject().stream()
				.filter(repository -> repository.getDomainObject().equals(domainObject)).findFirst()
				.map(repository -> repository.getMethodByType(RepositoryMethodTypeEnumeration.PERSIST)).orElse(null);

		if (repositoryMethod == null)
			throw new IllegalStateException(
					"The respective repository method for domain object '" + domainObject.getName() + "' could not be found!");

		if (paramIndex != 0 && !method.getBoundaryBean().getDomainObject().equals(domainObject))
			repositoryName = repositoryMethod.getRepository().getLowerCaseName() + ".";

		b.append("\n");

		final var checkParam = repositoryMethod.addUniqueCheck() ? ", true" : "";

		// If the method either returns the root DTO or other child domain objects should be created we must refresh the root domain
		// object!
		if (paramIndex == 0 && (returnType instanceof DTOBean || paramCount > 1)) {
			b.append(domainObjectName + " = " + repositoryName + repositoryMethod.getName());
			b.append("(" + domainObjectName + checkParam + ", true, true);\n");
		}
		else {
			if (addNewObjectToOneToMany)
				b.append(domainObjectName + " = ");

			b.append(repositoryName + repositoryMethod.getName() + "(" + domainObjectName + checkParam + ", false, false);\n");
		}

		boolean firstAttr = true;

		// If the method returns the root DTO we must initialize all fields that are filled automatically by the persistence provider!
		if (paramIndex == 0 && returnType instanceof DTOBean) {
			final var dto = (DTOBean) method.getReturnType();
			final DTOBeanAttribute pkAttr = dto.getPKAttribute();

			// Get the primary key if it is generated automatically!
			if (domainObject.getIDGenerator().getGeneratorType() != IDGeneratorTypeEnumeration.NONE) {
				final String setter = pkAttr.getSetterName();
				final String getter = pkAttr.getDomainAttribute().getGetterName();

				if (firstAttr) {
					firstAttr = false;
					b.append("\n");
				}

				b.append(returnObjectName + "." + setter + "(" + domainObjectName + "." + getter + ");\n");
			}

			// Change the values of the version and the last update fields of the DTO if they exist
			for (final DTOBeanAttribute attr : dto.getAttributes()) {
				if (attr.getDomainAttribute() == null || attr.getAssociation() != null)
					continue;

				if (attr.getDomainAttribute().isTrackVersion() || attr.getDomainAttribute().isSetDateOnPersist()) {
					final String setter = attr.getSetterName();
					final String getter = attr.getDomainAttribute().getGetterName();

					if (firstAttr) {
						firstAttr = false;
						b.append("\n");
					}

					b.append(returnObjectName + "." + setter + "(" + domainObjectName + "." + getter + ");\n");
				}
			}
		}

		return b.toString();
	}

}
