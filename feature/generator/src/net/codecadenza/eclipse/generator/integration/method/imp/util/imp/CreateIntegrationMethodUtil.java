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
package net.codecadenza.eclipse.generator.integration.method.imp.util.imp;

import static net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator.SERVICE_NAME_SUFFIX;
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_REPOSITORY;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import net.codecadenza.eclipse.generator.dto.DTOInlineConversionGenerator;
import net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.service.ServiceBean;

/**
 * <p>
 * Generator utility for facade methods that perform create operations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CreateIntegrationMethodUtil extends AbstractIntegrationMethodUtil {
	/**
	 * Constructor
	 * @param method
	 */
	public CreateIntegrationMethodUtil(AbstractIntegrationMethod method) {
		super(method);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil#createConversionFragment()
	 */
	@Override
	public String createConversionFragment() {
		final var b = new StringBuilder();
		final var entityNameSet = new HashSet<String>();
		int paramIndex = 0;
		int paramCount = 0;

		// Determine the number of parameters that represent a DTO
		for (final MethodParameter p : method.getMethodParameters())
			if (p.getType() instanceof DTOBean)
				paramCount++;

		for (final MethodParameter p : method.getMethodParameters()) {
			if (!(p.getType() instanceof final DTOBean dtoBean))
				continue;

			var domainObjName = "new" + dtoBean.getDomainObject().getName();

			// Avoid name clashes if the method uses domain objects of the same type!
			if (entityNameSet.contains(domainObjName))
				domainObjName += paramIndex;

			entityNameSet.add(domainObjName);

			final var converter = new DTOInlineConversionGenerator(type, dtoBean, domainObjName, p.getName());
			boolean addNewObjectToOneToMany = false;

			if (paramIndex == 0)
				b.append("// Create new object to be persisted\n");
			else
				b.append("\n");

			b.append("var " + domainObjName + " = new " + dtoBean.getDomainObject().getName() + "();\n");
			b.append(converter.addAttributeSetters());

			if (paramIndex > 0) {
				// Set the parent of the child object!
				final DomainObject parentDO = boundaryMethod.getBoundaryBean().getDomainObject();
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
				// Add the new object to the unidirectional one-to-many association
				final DomainObject parentDO = boundaryMethod.getBoundaryBean().getDomainObject();
				final DomainObject paramDO = dtoBean.getDomainObject();

				for (final AbstractDomainAssociation assoc : parentDO.getAllAssociations())
					if (assoc instanceof final OneToManyAssociation oneToMany && paramDO.equals(oneToMany.getTarget())
							&& !oneToMany.isBidirectional()) {
						b.append("new" + parentDO.getName() + "." + oneToMany.getGetterName() + ".add(" + domainObjName + ");\n");
						break;
					}
			}

			paramIndex++;
		}

		return b.toString();
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
	private String createPersistCall(String domainObjectName, DomainObject domainObject, boolean addNewObjectToOneToMany,
			int paramIndex, int paramCount) {
		final var b = new StringBuilder();
		final JavaType returnType = method.getReturnType();
		final RepositoryMethod repositoryMethod = project.getAllRepositoriesOfProject().stream()
				.filter(repository -> repository.getDomainObject().equals(domainObject)).findFirst()
				.map(repository -> repository.getMethodByType(RepositoryMethodTypeEnumeration.PERSIST)).orElse(null);

		if (repositoryMethod == null)
			throw new IllegalStateException("An appropriate repository method of type 'PERSIST' could not be found!");

		final var checkParam = repositoryMethod.addUniqueCheck() ? ", true" : "";
		var repositoryName = DEFAULT_REPOSITORY + ".";

		if (!domainObject.equals(boundaryMethod.getBoundaryBean().getDomainObject()))
			repositoryName = domainObject.getLowerCaseName() + SERVICE_NAME_SUFFIX + ".";

		b.append("\n");

		// If the method either returns the root DTO or other child domain objects should be created we must refresh the root domain
		// object!
		if (paramIndex == 0 && (returnType instanceof DTOBean || paramCount > 1)) {
			b.append(domainObjectName + " = " + repositoryName + repositoryMethod.getName() + "(");
			b.append(domainObjectName + checkParam + ", true, true);\n\n");
		}
		else {
			if (addNewObjectToOneToMany)
				b.append(domainObjectName + " = ");

			b.append(repositoryName + repositoryMethod.getName() + "(" + domainObjectName + checkParam + ", false, false);\n");
		}

		// If the method returns the root DTO we must initialize all fields that are filled automatically by the persistence provider!
		if (paramIndex == 0 && returnType instanceof DTOBean) {
			final var dto = (DTOBean) method.getReturnType();
			final DTOBeanAttribute pkAttr = dto.getPKAttribute();
			final String returnObjName = boundaryMethod.getMethodParameters().get(0).getName();

			// Get the primary key if it is generated automatically!
			if (domainObject.getIDGenerator().getGeneratorType() != IDGeneratorTypeEnumeration.NONE) {
				final String setter = pkAttr.getSetterName();
				final String getter = pkAttr.getDomainAttribute().getGetterName();

				b.append(returnObjName + "." + setter + "(" + domainObjectName + "." + getter + ");\n");
			}

			// Change the values of version and last update fields of the DTO if they exist
			for (final DTOBeanAttribute attr : dto.getAttributes()) {
				if (attr.getDomainAttribute() == null || attr.getAssociation() != null)
					continue;

				if (attr.getDomainAttribute().isTrackVersion() || attr.getDomainAttribute().isSetDateOnPersist()) {
					final String setter = attr.getSetterName();
					final String getter = attr.getDomainAttribute().getGetterName();

					b.append(returnObjName + "." + setter + "(" + domainObjectName + "." + getter + ");\n");
				}
			}

			b.append("\n");
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil#getServices()
	 */
	@Override
	public Map<String, ServiceBean> getServices() {
		final var serviceMap = new HashMap<String, ServiceBean>();

		// There might be additional facade beans that a persist method needs
		for (final MethodParameter p : method.getMethodParameters()) {
			if (!(p.getType() instanceof final DTOBean paramDTO))
				continue;

			if (paramDTO.getDomainObject().equals(method.getIntegrationBean().getDomainObject()))
				continue;

			final BoundaryBean boundaryBean = project.getBoundaryByDomainObject(paramDTO.getDomainObject());

			if (boundaryBean != null)
				serviceMap.put(paramDTO.getDomainObject().getLowerCaseName() + SERVICE_NAME_SUFFIX, boundaryBean);
		}

		return serviceMap;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = new HashSet<>(super.getImports());

		// There might be additional packages to be imported as a persist method can have more than one object to be persisted!
		for (final MethodParameter p : method.getMethodParameters()) {
			if (!(p.getType() instanceof final DTOBean paramDTO))
				continue;

			final var converter = new DTOInlineConversionGenerator(type, paramDTO, "", "");
			final String subNamespace = p.getType().getNamespace().getName();
			final String domainNamespace = project.getDomainNamespace().toString();
			final String dtoNamespace = project.getDTONamespace().toString();
			final String facadeNamespace = project.getBoundaryNamespace().toString();

			imports.addAll(converter.getImports());
			imports.add("import " + dtoNamespace + "." + subNamespace + ".*;");
			imports.add("import " + domainNamespace + "." + subNamespace + ".*;");
			imports.add("import " + facadeNamespace + "." + subNamespace + ".*;");
		}

		return imports;
	}

}
