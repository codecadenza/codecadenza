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
package net.codecadenza.eclipse.generator.facade.method;

import java.util.HashSet;
import java.util.Set;
import net.codecadenza.eclipse.generator.boundary.method.CreateBoundaryMethodGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.common.IMethodGeneratorUtility;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;

/**
 * <p>
 * Generator for facade methods that perform a create operation
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CreateFacadeMethodGenerator extends CreateBoundaryMethodGenerator {
	/**
	 * Constructor
	 * @param method
	 * @param utility
	 * @param parentGenerator
	 */
	public CreateFacadeMethodGenerator(BoundaryMethod method, IMethodGeneratorUtility utility,
			AbstractJavaSourceGenerator parentGenerator) {
		super(method, utility, parentGenerator);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.PersistBoundaryMethodGenerator#createMethodBody()
	 */
	@Override
	public String createMethodBody() {
		final var b = new StringBuilder();
		int paramIndex = 0;
		int paramCount = 0;

		// Determine the number of parameters that represent a DTO
		for (final MethodParameter p : method.getMethodParameters())
			if (p.getType() instanceof DTOBean)
				paramCount++;

		for (final MethodParameter p : method.getMethodParameters()) {
			if (!(p.getType() instanceof final DTOBean dtoBean))
				continue;

			final String domainObjName = p.getName();
			final String conversionFragment = new DomainObjectInlineConversionGenerator(method.getMethodType(), dtoBean, domainObjName,
					domainObjName).createConversion();
			boolean parentSet = false;
			boolean addNewObjectToOneToMany = false;

			if (paramIndex > 0) {
				b.append("\n");
				b.append("if(" + p.getName() + " != null)\n");
				b.append("{\n");
			}

			b.append(conversionFragment);

			if (paramIndex > 0) {
				// Set the parent of the child object!
				final DomainObject parentDO = method.getBoundaryBean().getDomainObject();
				final DomainObject paramDO = dtoBean.getDomainObject();

				for (final AbstractDomainAssociation assoc : parentDO.getAllAssociations())
					if (assoc instanceof final OneToManyAssociation oneToMany && paramDO.equals(oneToMany.getTarget())) {
						if (oneToMany.isBidirectional()) {
							final ManyToOneAssociation reverseAssoc = oneToMany.getReverseAssociation();

							b.append(domainObjName + "." + reverseAssoc.getSetterName() + "(" + returnObjectName + ");\n");
							parentSet = true;
						}
						else
							addNewObjectToOneToMany = true;

						break;
					}
			}

			if (parentSet || !conversionFragment.isEmpty())
				b.append("\n");

			// Invoke the persist method
			b.append(createPersistCall(domainObjName, dtoBean.getDomainObject(), addNewObjectToOneToMany, paramIndex, paramCount));

			if (addNewObjectToOneToMany) {
				// Add the new object to the respective unidirectional one-to-many association
				final DomainObject parentDO = method.getBoundaryBean().getDomainObject();
				final DomainObject paramDO = dtoBean.getDomainObject();

				for (final AbstractDomainAssociation assoc : parentDO.getAllAssociations())
					if (assoc instanceof final OneToManyAssociation oneToMany && paramDO.equals(oneToMany.getTarget())
							&& !oneToMany.isBidirectional()) {
						b.append(returnObjectName + "." + oneToMany.getGetterName() + ".add(" + domainObjName + ");\n");
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
	 * @return a set containing all necessary facades
	 */
	public Set<BoundaryBean> getAllFacades() {
		final var facadeMap = new HashSet<BoundaryBean>();

		// There might be additional facades that a persist method needs
		for (final MethodParameter p : method.getMethodParameters()) {
			if (!(p.getType() instanceof final DTOBean paramDTO))
				continue;

			if (paramDTO.getDomainObject().equals(method.getBoundaryBean().getDomainObject()))
				continue;

			final BoundaryBean boundaryBean = project.getBoundaryByDomainObject(paramDTO.getDomainObject());

			if (boundaryBean != null)
				facadeMap.add(boundaryBean);
		}

		return facadeMap;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.PersistBoundaryMethodGenerator#getLocalServices()
	 */
	@Override
	public String getLocalServices() {
		final var b = new StringBuilder();

		getAllFacades().forEach(bean -> {
			final String facadeName = bean.getInterfaceName().substring(0, 1).toLowerCase() + bean.getInterfaceName().substring(1);

			b.append("final var " + facadeName + " = new " + bean.getInterfaceName() + "(em);\n");
		});

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.PersistBoundaryMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = new HashSet<>(generatorUtility.getImports());

		if (method.addUniqueCheck())
			imports.add("import net.codecadenza.runtime.repository.*;");

		// There might be additional packages to be imported as a persist method can have more than one object to be persisted!
		for (final MethodParameter p : method.getMethodParameters()) {
			if (!(p.getType() instanceof final DTOBean dto))
				continue;

			final var converter = new DomainObjectInlineConversionGenerator(method.getMethodType(), dto, "", "");

			imports.addAll(converter.getImports());

			if (dto.getDomainObject().getNamespace().equals(method.getBoundaryBean().getDomainObject().getNamespace()))
				continue;

			final String subNamespace = p.getType().getNamespace().getName();
			final String domainNamespace = project.getDomainNamespace().toString();
			final String boundaryNamespace = project.getBoundaryNamespace().toString();

			imports.add("import " + domainNamespace + "." + subNamespace + ".*;");
			imports.add("import " + boundaryNamespace + "." + subNamespace + ".*;");
		}

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.PersistBoundaryMethodGenerator#getInterfaceImports()
	 */
	@Override
	public Set<String> getInterfaceImports() {
		return getImports();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.PersistBoundaryMethodGenerator#createPersistCall(java.lang.String,
	 * net.codecadenza.eclipse.model.domain.DomainObject, boolean, int, int)
	 */
	@Override
	protected String createPersistCall(String domainObjectName, DomainObject domainObject, boolean addNewObjectToOneToMany,
			int paramIndex, int paramCount) {
		final var b = new StringBuilder();
		final JavaType returnType = method.getReturnType();
		final RepositoryMethod repositoryMethod = project.getAllRepositoriesOfProject().stream()
				.filter(repository -> repository.getDomainObject().equals(domainObject)).findFirst()
				.map(repository -> repository.getMethodByType(RepositoryMethodTypeEnumeration.PERSIST)).orElse(null);

		if (repositoryMethod == null)
			throw new IllegalStateException("An appropriate repository method of type 'PERSIST' could not be found!");

		final var checkParam = repositoryMethod.addUniqueCheck() ? ", true" : "";

		if (paramIndex == 0) {
			if (returnType instanceof DTOBean || paramCount > 1) {
				b.append(domainObjectName + " = " + repositoryMethod.getName());
				b.append("(" + domainObjectName + checkParam + ", true, true);\n");
			}
			else
				b.append(repositoryMethod.getName() + "(" + domainObjectName + checkParam + ", false, false);\n");
		}
		else {
			final BoundaryBean boundaryBean = project.getBoundaryByDomainObject(domainObject);
			var facadeName = "";

			if (boundaryBean != null) {
				if (!method.getBoundaryBean().getDomainObject().equals(domainObject))
					facadeName = boundaryBean.getInterfaceName().substring(0, 1).toLowerCase()
							+ boundaryBean.getInterfaceName().substring(1) + ".";

				if (addNewObjectToOneToMany)
					b.append(domainObjectName + " = ");

				b.append(facadeName + repositoryMethod.getName() + "(" + domainObjectName + checkParam + ", false, false);\n");
			}
		}

		return b.toString();
	}

}
