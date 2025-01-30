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
package net.codecadenza.eclipse.generator.boundary;

import static net.codecadenza.eclipse.shared.Constants.DEFAULT_REPOSITORY;

import java.util.Collection;
import java.util.HashSet;
import net.codecadenza.eclipse.generator.boundary.method.BoundaryMethodGeneratorFactory;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.service.ServiceBean;

/**
 * <p>
 * Generator for boundary beans
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class BoundaryBeanGenerator extends AbstractJavaSourceGenerator {
	private final BoundaryBean boundaryBean;
	private final Project project;
	private final boolean hasMethods;
	private final HashSet<ServiceBean> addServiceSet = new HashSet<>();
	private boolean addLoggingInterceptor;
	private String beanName;

	/**
	 * Constructor
	 * @param boundaryBean
	 */
	public BoundaryBeanGenerator(BoundaryBean boundaryBean) {
		super(boundaryBean.getBeanSourceFile());

		this.boundaryBean = boundaryBean;
		this.project = boundaryBean.getNamespace().getProject();
		this.addLoggingInterceptor = false;
		this.beanName = boundaryBean.getInterfaceName();
		this.hasMethods = !boundaryBean.getBoundaryMethods().isEmpty();

		if (project.isAddBoundaryInterface())
			this.beanName = boundaryBean.getName();

		if (project.isJakartaEEApplication() && project.getDomainObjectByTag(DomainTagEnumeration.LOGGING) != null)
			this.addLoggingInterceptor = true;

		this.addServiceSet.add(boundaryBean.getRepository());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		if (project.isJakartaEEApplication()) {
			importPackage("jakarta.ejb");

			if (hasMethods)
				importPackage("jakarta.annotation.security");

			if (project.getDomainObjectByTag(DomainTagEnumeration.LOGGING) != null) {
				importPackage("jakarta.interceptor");
				importClass("net.codecadenza.runtime.server.logging.LoggingInterceptor");
			}
		}
		else if (project.isSpringBootApplication()) {
			importClass("org.springframework.stereotype.Service");

			if (hasMethods) {
				importPackage("jakarta.annotation.security");
				importPackage("org.springframework.transaction.annotation");
			}
		}
		else if (hasMethods) {
			importPackage("net.codecadenza.runtime.jpa");
			importPackage("jakarta.persistence");
		}

		if (hasMethods)
			importPackage(boundaryBean.getRepository().getNamespace().toString());

		if (project.isAddBoundaryInterface())
			importPackage(boundaryBean.getNamespace().toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		if (project.isJakartaEEApplication()) {
			b.append("@Stateless\n");

			if (addLoggingInterceptor)
				b.append("@Interceptors(LoggingInterceptor.class)\n");

			if (project.isAddBoundaryInterface()) {
				b.append("@Local");
				b.append("(" + boundaryBean.getInterfaceName() + ".class)\n");
			}
		}
		else if (project.isSpringBootApplication())
			b.append("@Service\n");

		b.append("public class " + beanName);

		if (project.isAddBoundaryInterface()) {
			b.append(" implements ");
			b.append(boundaryBean.getInterfaceName());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		final String subNamespace = boundaryBean.getDomainObject().getNamespace().getName();
		final String domainNamespace = project.getDomainNamespace().toString();
		final String dtoNamespace = project.getDTONamespace().toString();

		// Search for additional services and add necessary imports
		for (final BoundaryMethod method : boundaryBean.getBoundaryMethods()) {
			importPackage(dtoNamespace + "." + subNamespace);
			importPackage(domainNamespace + "." + subNamespace);

			addImports(BoundaryMethodGeneratorFactory.getMethodGenerator(method, this).getImports());

			if (project.isJavaSEApplication())
				continue;

			if (method.getMethodType() == BoundaryMethodTypeEnumeration.CREATE) {
				for (final MethodParameter p : method.getMethodParameters()) {
					if (!(p.getType() instanceof final DTOBean paramDTO))
						continue;

					final Collection<Repository> repositories = project.getAllRepositoriesOfProject();

					for (final Repository repository : repositories)
						if (repository.getDomainObject().equals(paramDTO.getDomainObject())) {
							importPackage(repository.getNamespace().toString());

							addServiceSet.add(repository);
							break;
						}
				}
			}
			else if (method.getMethodType() == BoundaryMethodTypeEnumeration.DOWNLOAD_EXPORT
					|| method.getMethodType() == BoundaryMethodTypeEnumeration.UPLOAD_IMPORT) {
				final var exchangeMethod = (DataExchangeMethod) method.getServiceMethod();
				final DataExchangeServiceBean exchangeService = exchangeMethod.getDataExchangeServiceBean();
				addServiceSet.add(exchangeService);
			}
		}

		// Add declarations for services that needs to be injected
		if (hasMethods && !project.isJavaSEApplication())
			addServiceSet.forEach(serviceBean -> {
				if (serviceBean.equals(boundaryBean.getRepository()))
					addPrivateField(serviceBean.getName(), DEFAULT_REPOSITORY).inject().create();
				else
					addPrivateField(serviceBean.getName(), serviceBean.getLowerCaseName()).inject().create();
			});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		boundaryBean.getBoundaryMethods().forEach(method -> {
			final var b = new StringBuilder();
			final String identifier = BoundaryMethodGeneratorFactory.getMethodGenerator(method, this).getMethodSignature(false);

			b.append(BoundaryMethodGeneratorFactory.getMethodGenerator(method, this).createComment());

			if (project.isAddBoundaryInterface())
				b.append("@Override\n");

			b.append(BoundaryMethodGeneratorFactory.getMethodGenerator(method, this).createMethod());

			addMethod(identifier, b.toString());
		});
	}

}
