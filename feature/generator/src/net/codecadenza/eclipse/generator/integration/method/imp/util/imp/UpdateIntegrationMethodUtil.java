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

import static net.codecadenza.eclipse.shared.Constants.DEFAULT_REPOSITORY;
import static net.codecadenza.eclipse.shared.Constants.EXISTING_OBJ_PREFIX;

import java.util.HashSet;
import java.util.Set;
import net.codecadenza.eclipse.generator.dto.DTOInlineConversionGenerator;
import net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;

/**
 * <p>
 * Generator utility for facade methods that perform update operations
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class UpdateIntegrationMethodUtil extends AbstractIntegrationMethodUtil {
	private DTOBean dtoBean;
	private String paramName;

	/**
	 * Constructor
	 * @param method
	 */
	public UpdateIntegrationMethodUtil(AbstractIntegrationMethod method) {
		super(method);

		// The first parameter represents the data transfer object that holds data for the update operation!
		for (final MethodParameter p : method.getMethodParameters())
			if (p.getType() instanceof final DTOBean dto) {
				this.dtoBean = dto;
				this.paramName = p.getName();
				break;
			}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil#createConversionFragment()
	 */
	@Override
	public String createConversionFragment() {
		final var b = new StringBuilder();
		final String domainObjectName = EXISTING_OBJ_PREFIX + dtoBean.getDomainObject().getUpperCaseName();
		final Repository repository = boundaryMethod.getBoundaryBean().getRepository();
		final var converter = new DTOInlineConversionGenerator(type, dtoBean, domainObjectName, paramName);

		// Search for an appropriate finder method
		final RepositoryMethod finderMethod = repository.getMethodByType(RepositoryMethodTypeEnumeration.FIND_EXISTING);

		if (finderMethod == null)
			throw new IllegalStateException("An appropriate repository method of type 'FIND_EXISTING' could not be found!");

		// Search for the merge method
		final RepositoryMethod mergeMethod = repository.getMethodByType(RepositoryMethodTypeEnumeration.MERGE);

		if (mergeMethod == null)
			throw new IllegalStateException("An appropriate repository method of type 'MERGE' could not be found!");

		final var checkParam = mergeMethod.addUniqueCheck() ? ", true" : "";

		// Search for the existing object
		b.append("// Find and attach object\n");
		b.append(dtoBean.getDomainObject().getName() + " " + domainObjectName + " = ");
		b.append(DEFAULT_REPOSITORY + "." + finderMethod.getName() + "(");
		b.append(paramName + "." + dtoBean.getPKAttribute().getGetterName() + ");\n\n");
		b.append(converter.addAttributeSetters());
		b.append("\n\n");

		if (method.getReturnType().equals(dtoBean)) {
			b.append(domainObjectName + " = " + DEFAULT_REPOSITORY + "." + mergeMethod.getName());
			b.append("(" + domainObjectName + checkParam + ", true);\n\n");

			// Change the values of version and last update fields of the DTO if they exist
			for (final DTOBeanAttribute attr : dtoBean.getAttributes())
				if (attr.getDomainAttribute() != null && attr.getAssociation() == null && attr.getDomainAttribute().isTrackVersion()) {
					final String setter = attr.getSetterName();
					final String getter = attr.getDomainAttribute().getGetterName();

					b.append(paramName + "." + setter + "(" + domainObjectName + "." + getter + ");\n");
					break;
				}

			for (final DTOBeanAttribute attr : dtoBean.getAttributes())
				if (attr.getDomainAttribute() != null && attr.getAssociation() == null && attr.getDomainAttribute().isSetDateOnUpdate()) {
					final String setter = attr.getSetterName();
					final String getter = attr.getDomainAttribute().getGetterName();

					b.append(paramName + "." + setter + "(" + domainObjectName + "." + getter + ");\n");
					break;
				}
		}
		else
			b.append(DEFAULT_REPOSITORY + "." + mergeMethod.getName() + "(" + domainObjectName + checkParam + ", false);\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = new HashSet<String>();
		final var converter = new DTOInlineConversionGenerator(type, dtoBean, "", "");

		imports.addAll(super.getImports());
		imports.addAll(converter.getImports());
		imports.add("import " + dtoBean.getDomainObject().getNamespace().toString() + ".*;");

		return imports;
	}

}
