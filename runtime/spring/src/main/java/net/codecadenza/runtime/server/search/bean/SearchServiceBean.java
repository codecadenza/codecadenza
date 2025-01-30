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
package net.codecadenza.runtime.server.search.bean;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import java.util.Collection;
import net.codecadenza.runtime.search.SearchService;
import org.springframework.stereotype.Repository;

/**
 * <p>
 * Implementation of a generic search service
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@Repository
public class SearchServiceBean implements SearchService {
	private static final int MAX_RESULTS = 20;
	private static final String PARAM_NAME = "paramPrefix";

	private @PersistenceContext EntityManager em;

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.search.SearchService#getListOfValues(java.lang.String, java.lang.String)
	 */
	@Override
	public Collection<String> getListOfValues(String command, String prefix) {
		final TypedQuery<String> query = em.createQuery(command, String.class);
		query.setParameter(PARAM_NAME, prefix + "%");
		query.setMaxResults(MAX_RESULTS);

		return query.getResultList();
	}

}
